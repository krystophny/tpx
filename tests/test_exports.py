#!/usr/bin/env python3
"""Exercise the actual TpX executable against independently defined drawings."""
import os
from pathlib import Path
import re
import shutil
import stat
import struct
import subprocess
import tempfile
import unittest
import zlib
import xml.etree.ElementTree as ET

BINARY = Path(os.environ["TPX_BINARY"]).resolve()
SVG = "{http://www.w3.org/2000/svg}"


class ExportTests(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory(prefix="tpx-test-")
        self.root = Path(self.directory.name)
        self.env = os.environ.copy()
        self.env.update(HOME=str(self.root / "home"),
                        USERPROFILE=str(self.root / "home"),
                        XDG_CONFIG_HOME=str(self.root / "config"),
                        XDG_STATE_HOME=str(self.root / "state"),
                        XDG_CACHE_HOME=str(self.root / "cache"),
                        APPDATA=str(self.root / "config"),
                        LOCALAPPDATA=str(self.root / "state"))

    def tearDown(self):
        self.directory.cleanup()

    def drawing(self, text=None):
        drawing = ET.Element("TpX", v="5", TeXFormat="tikz",
                             PdfTeXFormat="tikz", TeXFigure="none",
                             PicScale="1", Border="2")
        ET.SubElement(drawing, "line", x1="0", y1="0", x2="20", y2="10")
        ET.SubElement(drawing, "rect", x="0", y="0", w="20", h="10")
        if text is not None:
            ET.SubElement(drawing, "text", x="0", y="15", h="5", t=text)
        ET.SubElement(drawing, "caption", label="literal &lt;").text = "literal &lt; &amp;"
        ET.SubElement(drawing, "comment").text = "literal &#9;"
        source = self.root / "input.TpX"
        xml = ET.tostring(drawing, encoding="unicode")
        source.write_text("\n".join("%" + line for line in xml.splitlines()) + "\n")
        return source

    def run_tpx(self, *arguments, expect_success=True):
        result = subprocess.run([str(BINARY), *map(str, arguments)],
                                cwd=self.root, capture_output=True,
                                env=self.env, text=True, timeout=20)
        if expect_success:
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        else:
            self.assertNotEqual(result.returncode, 0, result.stdout)
        return result

    def save_leftovers(self):
        return sorted(path.name for path in self.root.iterdir()
                      if path.suffix == ".tmp" or path.name.startswith("tpx-stage"))

    @staticmethod
    def read_tpx(path):
        lines = path.read_text().splitlines()
        return ET.fromstring("\n".join(line[1:] for line in lines
                                       if line.startswith("%")))

    def test_svg_preserves_namespace_and_geometry(self):
        self.run_tpx("-f", self.drawing(), "-x", "svg", "-o", "drawing")
        drawing = ET.parse(self.root / "drawing.svg").getroot()
        self.assertEqual(drawing.tag, SVG + "svg")
        rectangle = drawing.find(SVG + "rect")
        self.assertIsNotNone(rectangle)
        self.assertEqual(float(rectangle.get("width")), 20)
        self.assertEqual(float(rectangle.get("height")), 10)
        line = drawing.find(SVG + "polyline")
        points = [tuple(map(float, point.split(",")))
                  for point in line.get("points").split()]
        self.assertEqual(len(points), 2)
        for actual, expected in zip(points, [(0, 0), (20, -10)]):
            self.assertAlmostEqual(actual[0], expected[0], places=4)
            self.assertAlmostEqual(actual[1], expected[1], places=4)

    def test_numeric_character_references(self):
        source = self.drawing("Z")
        source.write_text(source.read_text().replace('t="Z"',
                          't="&#09;&#x3B2;&#128578;"'))
        self.run_tpx("-f", source, "-o", "numeric.TpX")
        saved = self.read_tpx(self.root / "numeric.TpX")
        self.assertEqual(saved.find("text").get("t"), "\tβ🙂")

    def test_native_save_preserves_tex_centering(self):
        for centered in ("0", "1"):
            with self.subTest(centered=centered):
                source = self.drawing()
                drawing = self.read_tpx(source)
                drawing.set("TeXCenterFigure", centered)
                xml = ET.tostring(drawing, encoding="unicode")
                source.write_text("\n".join("%" + line for line in xml.splitlines()) + "\n")
                first = self.root / ("first-" + centered + ".TpX")
                second = self.root / ("second-" + centered + ".TpX")
                self.run_tpx("-f", source, "-o", first)
                self.assertEqual(self.read_tpx(first).get("TeXCenterFigure"), centered)
                self.run_tpx("-f", first, "-o", second)
                self.assertEqual(second.read_bytes(), first.read_bytes())

    def test_tikz_includes_png_without_eps_converter(self):
        def chunk(kind, data):
            return (struct.pack(">I", len(data)) + kind + data +
                    struct.pack(">I", zlib.crc32(kind + data)))
        (self.root / "photo.png").write_bytes(
            b"\x89PNG\r\n\x1a\n" +
            chunk(b"IHDR", struct.pack(">IIBBBBB", 2, 1, 8, 2, 0, 0, 0)) +
            chunk(b"IDAT", zlib.compress(b"\x00\xff\x00\x00\x00\x00\xff")) +
            chunk(b"IEND", b""))
        drawing = ET.Element("TpX", v="5", TeXFormat="tikz",
                             PdfTeXFormat="tikz", TeXFigure="none")
        ET.SubElement(drawing, "bitmap", x="10", y="20", w="40", h="20",
                      link="photo.png")
        source = self.root / "photo-figure.TpX"
        source.write_text("%" + ET.tostring(drawing, encoding="unicode") + "\n")
        empty = self.root / "empty-path"
        empty.mkdir()
        self.env["PATH"] = str(empty)
        self.run_tpx("-f", source, "-o", "saved.TpX")
        tail = [line for line in (self.root / "saved.TpX").read_text().splitlines()
                if not line.startswith("%")]
        nodes = [line for line in tail if "\\includegraphics" in line]
        self.assertEqual(len(nodes), 1, "\n".join(tail))
        self.assertIn("{photo}", nodes[0])
        self.assertIn("(10.00,20.00)", nodes[0])
        self.assertFalse((self.root / "photo.eps").exists())

    def test_saved_drawing_links_images_in_place(self):
        def chunk(kind, data):
            return (struct.pack(">I", len(data)) + kind + data +
                    struct.pack(">I", zlib.crc32(kind + data)))
        png = (b"\x89PNG\r\n\x1a\n" +
               chunk(b"IHDR", struct.pack(">IIBBBBB", 2, 1, 8, 2, 0, 0, 0)) +
               chunk(b"IDAT", zlib.compress(b"\x00\xff\x00\x00\x00\x00\xff")) +
               chunk(b"IEND", b""))
        figures = self.root / "project" / "figures"
        figures.mkdir(parents=True)
        (self.root / "elsewhere").mkdir()
        (figures / "quarter_torus.png").write_bytes(png)
        (self.root / "project" / "shared.png").write_bytes(png)
        drawing = ET.Element("TpX", v="5", TeXFormat="none",
                             PdfTeXFormat="none", TeXFigure="none")
        for x, image in (("10", figures / "quarter_torus.png"),
                         ("80", self.root / "project" / "shared.png")):
            ET.SubElement(drawing, "bitmap", x=x, y="10", w="60", h="30",
                          link=str(image))
        source = self.root / "elsewhere" / "src.TpX"
        source.write_text("%" + ET.tostring(drawing, encoding="unicode") + "\n")
        self.run_tpx("-f", source, "-o", figures / "fig.TpX")
        links = [item.get("link")
                 for item in self.read_tpx(figures / "fig.TpX").iter("bitmap")]
        self.assertEqual(links, ["quarter_torus.png",
                                 "../shared.png"])
        self.assertEqual([path for path in self.root.rglob("bitmaps")], [])

    def bitmap_only_drawing(self):
        """Write a 30x20 RGB PNG and a drawing showing only that image."""
        def chunk(kind, data):
            return (struct.pack(">I", len(data)) + kind + data +
                    struct.pack(">I", zlib.crc32(kind + data)))
        width, height = 30, 20
        pixels = bytes(value for y in range(height) for x in range(width)
                       for value in (8 * x, 12 * y, 255 if (x + y) % 2 else 40))
        rows = b"".join(b"\x00" + pixels[3 * width * y:3 * width * (y + 1)]
                        for y in range(height))
        (self.root / "photo.png").write_bytes(
            b"\x89PNG\r\n\x1a\n" +
            chunk(b"IHDR", struct.pack(">IIBBBBB", width, height, 8, 2, 0, 0, 0)) +
            chunk(b"IDAT", zlib.compress(rows)) + chunk(b"IEND", b""))
        drawing = ET.Element("TpX", v="5", TeXFormat="none",
                             PdfTeXFormat="none", TeXFigure="none")
        ET.SubElement(drawing, "bitmap", x="10", y="20", w="60", h="40",
                      link="photo.png")
        source = self.root / "bitmap.TpX"
        source.write_text("%" + ET.tostring(drawing, encoding="unicode") + "\n")
        # No external converter may be found.
        empty = self.root / "empty-path"
        empty.mkdir()
        self.env["PATH"] = str(empty)
        return source, pixels

    def test_pdf_embeds_bitmap_pixels(self):
        source, pixels = self.bitmap_only_drawing()
        self.run_tpx("-f", source, "-x", "pdf", "-o", "bitmap")
        pdf = (self.root / "bitmap.pdf").read_bytes()
        match = re.search(rb"<<((?:(?!>>).)*?/Subtype\s*/Image(?:(?!>>).)*)>>"
                          rb"\s*stream\r?\n", pdf, re.S)
        self.assertIsNotNone(match, "PDF has no image XObject")
        attributes = match.group(1)
        self.assertRegex(attributes, rb"/Width\s+30\b")
        self.assertRegex(attributes, rb"/Height\s+20\b")
        self.assertRegex(attributes, rb"/ColorSpace\s*/DeviceRGB")
        length = int(re.search(rb"/Length\s+(\d+)", attributes).group(1))
        data = pdf[match.end():match.end() + length]
        if b"FlateDecode" in attributes:
            data = zlib.decompress(data)
        self.assertEqual(data, pixels)

    def test_eps_embeds_bitmap_without_converter(self):
        source, pixels = self.bitmap_only_drawing()
        self.run_tpx("-f", source, "-x", "eps", "-o", "bitmap")
        lines = (self.root / "bitmap.eps").read_text().splitlines()
        start = next(index for index, line in enumerate(lines)
                     if "colorimage" in line) + 1
        data = bytes.fromhex("".join(lines[start:start + 20]))
        self.assertEqual(data, pixels)

    def test_saved_drawings_preserve_geometry_and_text(self):
        for text in ["", "z", "ordinary trailing text", "123.45",
                     'a&<>"\'\tZ', "alpha β and é", "<>&", "literal &amp; &lt; &#9;"]:
            with self.subTest(text=text):
                self.run_tpx("-f", self.drawing(text), "-o", "saved.TpX")
                saved = self.read_tpx(self.root / "saved.TpX")
                self.assertEqual(saved.get("v"), "5")
                self.assertEqual(saved.find("caption").text.strip(), "literal &lt; &amp;")
                self.assertEqual(saved.find("caption").get("label"), "literal &lt;")
                self.assertEqual(saved.find("comment").text.strip(), "literal &#9;")
                self.assertEqual(saved.find("rect").get("w"), "20")
                self.assertEqual(saved.find("line").get("x2"), "20")
                self.assertEqual(saved.find("text").get("t"), text)
                self.run_tpx("-f", "saved.TpX", "-o", "reloaded.TpX")
                reloaded = self.read_tpx(self.root / "reloaded.TpX")
                self.assertEqual(reloaded.find("text").get("t"), text)
                self.assertEqual(reloaded.find("rect").get("h"), "10")

    def test_comments_inside_groups_are_skipped(self):
        source = self.root / "comments.TpX"
        source.write_text(
            '%<TpX v="5" PicScale="1">\n'
            '%<group><!-- leading note --><line x1="0" y1="0" x2="10" y2="10"/>'
            '<!-- between --><line x1="0" y1="10" x2="10" y2="0"/></group>\n'
            '%</TpX>\n')
        self.run_tpx("-f", source, "-o", "saved.TpX")
        saved = self.read_tpx(self.root / "saved.TpX")
        self.assertEqual(len(saved.findall("./group/line")), 2)
        # Text between elements, as in a TpX block written on one line.
        source.write_text(
            '%<TpX v="5" PicScale="1">%  <group><line x1="0" y1="0" x2="10" y2="10"/>'
            '<line x1="0" y1="10" x2="10" y2="0"/></group>%</TpX>')
        self.run_tpx("-f", source, "-o", "saved.TpX")
        saved = self.read_tpx(self.root / "saved.TpX")
        self.assertEqual(len(saved.findall("./group/line")), 2)

    def test_cli_saves_tikz_by_output_extension(self):
        self.run_tpx("-f", self.drawing(), "-o", "drawing.tikz")
        tikz = (self.root / "drawing.tikz").read_text()
        self.assertNotIn("%<TpX", tikz)
        self.assertIn("\\begin{tikzpicture}", tikz)
        self.assertIn("rectangle", tikz)
        self.run_tpx("-f", "drawing.tikz", "-o", "from-tikz.TpX")
        rectangle = self.read_tpx(self.root / "from-tikz.TpX").find("rect")
        self.assertEqual(float(rectangle.get("w")), 20)

    def test_cli_opens_svg_like_the_gui(self):
        self.run_tpx("-f", self.drawing(), "-x", "svg", "-o", "drawing")
        self.run_tpx("-f", "drawing.svg", "-o", "from-svg.TpX")
        rectangle = self.read_tpx(self.root / "from-svg.TpX").find("rect")
        self.assertEqual(float(rectangle.get("w")), 20)

    def test_cli_rejects_unsupported_output_clearly(self):
        source = self.drawing()
        result = self.run_tpx("-f", source, "-o", "drawing.pdf", expect_success=False)
        self.assertIn("drawing.pdf", result.stderr)
        self.assertFalse((self.root / "drawing.pdf").exists())
        result = self.run_tpx("-f", source, "-x", "emf", expect_success=False)
        self.assertIn('Export format "emf"', result.stderr)
        result = self.run_tpx("-f", "missing.TpX", "-o", "out.TpX",
                              expect_success=False)
        self.assertIn("missing.TpX", result.stderr)

    @unittest.skipUnless(shutil.which("gs"), "Ghostscript is not installed")
    def test_cli_png_export(self):
        self.run_tpx("-f", self.drawing(), "-x", "png", "-o", "drawing")
        image = (self.root / "drawing.png").read_bytes()
        self.assertEqual(image[:8], b"\x89PNG\r\n\x1a\n")
        width, height = struct.unpack(">II", image[16:24])
        self.assertGreater(width, height)

    def test_saves_leave_no_temporary_files(self):
        source = self.drawing()
        target = self.root / "saved.TpX"
        self.run_tpx("-f", source, "-o", target)
        self.run_tpx("-f", source, "-o", target)
        self.assertEqual(self.save_leftovers(), [])
        (self.root / "folder.TpX").mkdir()
        self.run_tpx("-f", source, "-o", "folder.TpX", expect_success=False)
        self.assertEqual(self.save_leftovers(), [])

    @unittest.skipIf(os.name == "nt", "POSIX file modes")
    def test_saved_file_modes_follow_umask_and_existing_file(self):
        source = self.drawing()
        target = self.root / "saved.TpX"
        previous = os.umask(0o022)
        try:
            self.run_tpx("-f", source, "-o", target)
            self.assertEqual(stat.S_IMODE(target.stat().st_mode), 0o644)
            target.chmod(0o640)
            self.run_tpx("-f", source, "-o", target)
            self.assertEqual(stat.S_IMODE(target.stat().st_mode), 0o640)
        finally:
            os.umask(previous)


if __name__ == "__main__":
    unittest.main(verbosity=2)
