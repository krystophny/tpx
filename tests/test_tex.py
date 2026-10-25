#!/usr/bin/env python3
"""Compile actual exports and inspect TeX's evaluated dimensions."""
import os
from pathlib import Path
import re
import shutil
import subprocess
import struct
import tempfile
import unittest
import xml.etree.ElementTree as ET

import headless_display

headless_display.ensure_private_display()

BINARY = Path(os.environ["TPX_BINARY"]).resolve()


@unittest.skipUnless(shutil.which("pdflatex"), "pdflatex is not installed")
class TeXTests(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory(prefix="tpx-tex-")
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

    def run_command(self, command):
        result = subprocess.run(list(map(str, command)), cwd=self.root,
                                env=self.env, capture_output=True, text=True,
                                timeout=40)
        self.assertEqual(result.returncode, 0, result.stdout[-4000:] + result.stderr[-2000:])
        return result

    def drawing(self, **attributes):
        drawing = ET.Element("TpX", v="5", TeXFormat="tikz", PdfTeXFormat="tikz",
                             TeXFigure="none", PicScale="1", Border="2")
        drawing.attrib.update(attributes)
        ET.SubElement(drawing, "line", x1="0", y1="0", x2="20", y2="10", lw="2")
        ET.SubElement(drawing, "line", x1="0", y1="10", x2="20", y2="0", li="dash")
        ET.SubElement(drawing, "line", x1="0", y1="5", x2="20", y2="5", li="dot")
        ET.SubElement(drawing, "text", x="0", y="15", h="5", t="$(a+b)$")
        ET.SubElement(drawing, "text", x="0", y="25", h="10", t="Price $5 & tax")
        ET.SubElement(drawing, "text", x="0", y="35", h="5", t="Plain", tex=r"$x^2$")
        source = self.root / "input.TpX"
        source.write_text("%" + ET.tostring(drawing, encoding="unicode") + "\n")
        return source

    def cropped_drawing(self, figure):
        source = self.drawing(TeXFigure=figure, TeXCenterFigure="1")
        drawing = ET.fromstring(source.read_text()[1:])
        for child in list(drawing):
            drawing.remove(child)
        ET.SubElement(drawing, "rect", x="0", y="0", w="20", h="10")
        ET.SubElement(drawing, "caption", label="fig:test").text = "A drawing caption"
        source.write_text("%" + ET.tostring(drawing, encoding="unicode") + "\n")
        return source

    def assert_cropped_pdf(self, pdf):
        image = self.root / "page.png"
        self.run_command(["gs", "-q", "-dSAFER", "-dBATCH", "-dNOPAUSE",
                          "-sDEVICE=pnggray", "-r72", "-sOutputFile=" + str(image), pdf])
        data = image.read_bytes()
        self.assertEqual(data[:8], b"\x89PNG\r\n\x1a\n")
        width, height = struct.unpack(">II", data[16:24])
        # A 20x10 mm drawing with a 2 mm border is about 68x40 pt, not A4.
        self.assertTrue(65 <= width <= 73, (width, height))
        self.assertTrue(36 <= height <= 45, (width, height))

    @unittest.skipUnless(shutil.which("gs") and shutil.which("sam2p"),
                         "Ghostscript and real sam2p are required")
    def test_modern_sam2p_bitmap_survives_tikz_and_pdf_export(self):
        # Independently define a 64x64 red, uncompressed 24-bit BMP.
        pixels = b"\x00\x00\xff" * (64 * 64)
        bmp = (b"BM" + struct.pack("<IHHI", 54 + len(pixels), 0, 0, 54)
               + struct.pack("<IiiHHIIiiII", 40, 64, 64, 1, 24, 0,
                             len(pixels), 2835, 2835, 0, 0) + pixels)
        (self.root / "red.bmp").write_bytes(bmp)
        drawing = ET.Element("TpX", v="5", TeXFormat="tikz", PdfTeXFormat="tikz",
                             TeXFigure="none", PicScale="1", Border="2")
        ET.SubElement(drawing, "bitmap", x="0", y="0", w="16", h="16", link="red.bmp")
        source = self.root / "input.TpX"
        source.write_text("%" + ET.tostring(drawing, encoding="unicode") + "\n")
        # Seed the historical beside-executable settings file for migration.
        binary = self.root / BINARY.name
        shutil.copy2(BINARY, binary)
        for filename in ("preview.tex.inc", "metapost.tex.inc"):
            shutil.copy2(Path(__file__).resolve().parents[1] / "ci" / filename,
                         self.root / filename)
        (self.root / "TpX.ini").write_text("LineWidth_Default=1\nBitmap2EpsPath="
                                           + shutil.which("sam2p") + "\n")
        self.run_command([binary, "-f", source, "-o", self.root / "bitmap.TpX"])
        self.assertNotIn(b"%%BeginData:", (self.root / "red.eps").read_bytes())
        self.run_command([binary, "-f", self.root / "bitmap.TpX", "-x", "pdflatexsrc",
                          "-o", self.root / "bitmap-pdf"])
        self.run_command(["pdflatex", "-halt-on-error", "-interaction=nonstopmode", "bitmap-pdf.tex"])
        image = self.root / "bitmap.ppm"
        self.run_command(["gs", "-q", "-dSAFER", "-dBATCH", "-dNOPAUSE",
                          "-sDEVICE=ppmraw", "-r72", "-sOutputFile=" + str(image),
                          "bitmap-pdf.pdf"])
        data = image.read_bytes()
        # PPM has an ASCII header; comments can appear between header tokens.
        tokens, offset = [], 0
        while len(tokens) < 4:
            while data[offset:offset + 1].isspace():
                offset += 1
            if data[offset:offset + 1] == b"#":
                offset = data.index(b"\n", offset) + 1
                continue
            start = offset
            while not data[offset:offset + 1].isspace():
                offset += 1
            tokens.append(data[start:offset])
        self.assertEqual(tokens[0], b"P6")
        self.assertEqual(tokens[3], b"255")
        rgb = data[offset + 1:]
        red_count = sum(rgb[i] > 240 and rgb[i + 1] < 10 and rgb[i + 2] < 10
                        for i in range(0, len(rgb), 3))
        print(f"Bitmap PDF oracle: {red_count} red pixels", flush=True)
        # A 16mm square at 72dpi covers roughly 45x45 pixels. A missing bitmap
        # leaves a white page and zero red pixels, independent of TeX syntax.
        self.assertGreater(red_count, 1500, "The rendered PDF lost the red bitmap")
        self.assertIn(r"\includegraphics", (self.root / "bitmap.TpX").read_text())
        self.assertIn(r"\includegraphics", (self.root / "bitmap-pdf(TpX).tpx").read_text())

    @unittest.skipUnless(shutil.which("gs"), "Ghostscript is not installed")
    def test_exported_pdflatex_source_has_a_cropped_page(self):
        for figure in ("none", "figure"):
            with self.subTest(figure=figure):
                self.run_command([BINARY, "-f", self.cropped_drawing(figure),
                                  "-x", "pdflatexsrc", "-o", "cropped"])
                self.run_command(["pdflatex", "-halt-on-error", "-interaction=nonstopmode",
                                  "cropped.tex"])
                self.assert_cropped_pdf(self.root / "cropped.pdf")

    @unittest.skipUnless(shutil.which("latex") and shutil.which("dvips") and
                         shutil.which("gs"), "LaTeX/dvips/Ghostscript are not installed")
    def test_latex_pdf_export_handles_floating_figures(self):
        self.run_command([BINARY, "-f", self.cropped_drawing("figure"),
                          "-x", "latexpdf", "-o", "direct"])
        self.assertTrue((self.root / "direct.pdf").exists(), "LaTeX PDF export produced no file")
        self.assert_cropped_pdf(self.root / "direct.pdf")

    def test_tikz_defaults_are_customizable_and_math_is_preserved(self):
        self.run_command([BINARY, "-f", self.drawing(), "-o", "drawing.TpX"])
        # Hooks observe the dimensions TikZ and LaTeX actually use.
        hooks = r"""
\documentclass{article}
\usepackage{tikz}
\makeatletter
\let\tpxOldLineWidth\pgfsetlinewidth
\def\pgfsetlinewidth#1{\tpxOldLineWidth{#1}\typeout{TPX-STROKE:\the\pgflinewidth}}
\let\tpxOldSelectFont\selectfont
\def\selectfont{\tpxOldSelectFont\typeout{TPX-FONT:\f@size}}
\makeatother
"""
        for override, expected_width, expected_fonts in [
                ("", 1.707, (14.226, 28.453)),
                (r"\newcommand{\tpxLineWidth}{0.8mm}\newcommand{\tpxTextSize}{10pt}",
                 4.552, (10.0, 20.0))]:
            with self.subTest(override=bool(override)):
                (self.root / "document.tex").write_text(
                    hooks + override + r"\begin{document}\input{drawing.TpX}" +
                    (r"\ifdefined\tpxLineWidth\errmessage{Defaults leaked}\fi"
                     if not override else "") + r"\end{document}")
                self.run_command(["pdflatex", "-halt-on-error", "-interaction=nonstopmode",
                                  "document.tex"])
                log = (self.root / "document.log").read_text()
                widths = [float(x) for x in re.findall(r"TPX-STROKE:([\d.]+)pt", log)]
                fonts = [float(x) for x in re.findall(r"TPX-FONT:([\d.]+)", log)]
                self.assertTrue(any(abs(x - expected_width) < 0.02 for x in widths), widths)
                for expected in expected_fonts:
                    self.assertTrue(any(abs(x - expected) < 0.02 for x in fonts), fonts)
                self.assertGreater((self.root / "document.pdf").stat().st_size, 1000)
        exported = (self.root / "drawing.TpX").read_text().split(r"\begin{tikzpicture}", 1)[1]
        self.assertIn("$(a+b)$", exported)
        self.assertIn(r"Price \$5 \& tax", exported)
        self.assertIn(r"$x^2$", exported)

    def ink_grid(self, pdf, name, cells=(48, 24)):
        """Ink coverage of the inked area of a PDF page on a coarse grid."""
        image = self.root / (name + ".pgm")
        self.run_command(["gs", "-q", "-dSAFER", "-dBATCH", "-dNOPAUSE",
                          "-sDEVICE=pgmraw", "-r200", "-dTextAlphaBits=4",
                          "-dGraphicsAlphaBits=4", "-sOutputFile=" + str(image), pdf])
        artifacts = os.environ.get("TPX_TEX_ARTIFACTS")
        if artifacts and shutil.which("gs"):
            subprocess.run(["gs", "-q", "-dSAFER", "-dBATCH", "-dNOPAUSE",
                            "-sDEVICE=png16m", "-r150",
                            "-sOutputFile=" + str(Path(artifacts) / (name + ".png")),
                            str(self.root / pdf)], check=True)
        data = image.read_bytes()
        header = re.sub(rb"#[^\n]*\n", b"", data[:200]).split()
        width, height = int(header[1]), int(header[2])
        pixels = data[-width * height:]
        rows = [pixels[y * width:(y + 1) * width] for y in range(height)]
        ink = [(x, y) for y, row in enumerate(rows) for x, v in enumerate(row) if v < 200]
        self.assertGreater(len(ink), 1000, name + " rendered almost nothing")
        x0 = min(x for x, _ in ink); x1 = max(x for x, _ in ink) + 1
        y0 = min(y for _, y in ink); y1 = max(y for _, y in ink) + 1
        grid = [[0.0] * cells[0] for _ in range(cells[1])]
        counts = [[0] * cells[0] for _ in range(cells[1])]
        for y in range(y0, y1):
            row = rows[y]
            gy = (y - y0) * cells[1] // (y1 - y0)
            for x in range(x0, x1):
                gx = (x - x0) * cells[0] // (x1 - x0)
                counts[gy][gx] += 1
                grid[gy][gx] += (255 - row[x]) / 255
        return (x1 - x0) / (y1 - y0), [[g / max(c, 1) for g, c in zip(gr, cr)]
                                       for gr, cr in zip(grid, counts)]

    @unittest.skipUnless(shutil.which("gs") and shutil.which("rsvg-convert"),
                         "Ghostscript or rsvg-convert is not installed")
    def test_3d_figure_renders_alike_from_tikz_and_svg(self):
        example = Path(__file__).resolve().parents[1] / "examples" / "basic-3d.TpX"
        self.run_command([BINARY, "-f", example, "-o", "scene.TpX"])
        self.run_command([BINARY, "-f", example, "-x", "svg", "-o", "scene"])
        svg = (self.root / "scene.svg").read_text()
        self.assertEqual(svg.count("<tpx:solid"), 7)
        self.run_command(["rsvg-convert", "-f", "pdf", "-o", "scene-svg.pdf",
                          "scene.svg"])
        (self.root / "viatikz.tex").write_text(
            "\\documentclass{standalone}\\usepackage{tikz}\\begin{document}"
            "\\input{scene.TpX}\\end{document}\n")
        (self.root / "viasvg.tex").write_text(
            "\\documentclass{standalone}\\usepackage{graphicx}\\begin{document}"
            "\\includegraphics{scene-svg.pdf}\\end{document}\n")
        for stem in ("viatikz", "viasvg"):
            self.run_command(["pdflatex", "-halt-on-error",
                              "-interaction=nonstopmode", stem + ".tex"])
        aspect_tikz, tikz = self.ink_grid("viatikz.pdf", "basic-3d-tikz")
        aspect_svg, svg_grid = self.ink_grid("viasvg.pdf", "basic-3d-svg")
        self.assertAlmostEqual(aspect_tikz, aspect_svg, delta=0.03 * aspect_tikz)
        cells = [(a, b) for ra, rb in zip(tikz, svg_grid) for a, b in zip(ra, rb)]
        difference = sum(abs(a - b) for a, b in cells) / len(cells)
        coverage = sum(a for a, _ in cells) / len(cells)
        # Same faces, edges and hatching: per-cell ink differs only by
        # antialiasing and line-width rounding.
        self.assertLess(difference, 0.25 * coverage, (difference, coverage))

    @unittest.skipUnless(shutil.which("gs"), "Ghostscript is not installed")
    def test_tikz_fill_opacity_renders_translucent(self):
        source = self.root / "opacity.TpX"
        source.write_text(
            '%<TpX v="5" PicScale="1" TeXFormat="tikz" PdfTeXFormat="tikz" TeXFigure="none" Border="0">\n'
            '%<rect x="0" y="0" w="20" h="10" li="none" fill="#000000" fo="0.5"/>\n'
            '%<rect x="20" y="0" w="20" h="10" li="none" fill="#000000"/>\n'
            '%</TpX>\n')
        self.run_command([BINARY, "-f", source, "-o", "saved.TpX"])
        (self.root / "opacity.tex").write_text(
            "\\documentclass{standalone}\\usepackage{tikz}\\begin{document}"
            "\\input{saved.TpX}\\end{document}\n")
        self.run_command(["pdflatex", "-halt-on-error", "-interaction=nonstopmode",
                          "opacity.tex"])
        image = self.root / "opacity.pgm"
        self.run_command(["gs", "-q", "-dSAFER", "-dBATCH", "-dNOPAUSE",
                          "-sDEVICE=pgmraw", "-r72", "-sOutputFile=" + str(image),
                          "opacity.pdf"])
        data = image.read_bytes()
        header = re.sub(rb"#[^\n]*\n", b"", data[:200]).split()
        width, height = int(header[1]), int(header[2])
        pixels = data[-width * height:]
        middle = pixels[(height // 2) * width:(height // 2 + 1) * width]
        # Half-opaque black over the white page is mid gray; the opaque
        # reference rectangle stays black.
        self.assertLess(abs(middle[width // 4] - 128), 24, middle[width // 4])
        self.assertLess(middle[3 * width // 4], 24)

    @unittest.skipUnless(shutil.which("gs"), "Ghostscript is not installed")
    def test_tikz_radial_fade_renders(self):
        # A black disc that fades to transparent towards its edge.
        source = self.root / "blob.TpX"
        source.write_text(
            '%<TpX v="5" PicScale="1" TeXFormat="tikz" PdfTeXFormat="tikz" '
            'TeXFigure="none" Border="0">\n'
            '%<rect x="0" y="0" w="40" h="40" li="none" fill="#FFFFFF"/>\n'
            '%<circle x="20" y="20" d="40" li="none" fill="#000000" '
            'grad="radial" go="0"/>\n'
            '%</TpX>\n')
        self.run_command([BINARY, "-f", source, "-o", "saved.TpX"])
        (self.root / "blob.tex").write_text(
            "\\documentclass{standalone}\\usepackage{tikz}\\begin{document}"
            "\\input{saved.TpX}\\end{document}\n")
        self.run_command(["pdflatex", "-halt-on-error", "-interaction=nonstopmode",
                          "blob.tex"])
        image = self.root / "blob.pgm"
        self.run_command(["gs", "-q", "-dSAFER", "-dBATCH", "-dNOPAUSE",
                          "-sDEVICE=pgmraw", "-r72", "-sOutputFile=" + str(image),
                          "blob.pdf"])
        data = image.read_bytes()
        header = re.sub(rb"#[^\n]*\n", b"", data[:200]).split()
        width, height = int(header[1]), int(header[2])
        row = data[-width * height:][(height // 2) * width:(height // 2 + 1) * width]
        # Opaque core, half transparent at half the radius, clear at the rim.
        self.assertLess(row[width // 2], 24, row[width // 2])
        self.assertLess(abs(row[width // 4] - 128), 30, row[width // 4])
        self.assertGreater(row[2], 230, row[2])

    def test_scientific_examples_compile_with_tikz(self):
        examples = Path(__file__).resolve().parents[1] / "examples"
        expected_math = {
            "quadratic-function.TpX": r"$f(x)=x^2-1$",
            "vector-addition.TpX": r"$\vec{R}=\vec{A}+\vec{B}$",
            "double-slit.TpX": r"$\Delta r=0$",
        }
        for filename, formula in expected_math.items():
            with self.subTest(example=filename):
                stem = Path(filename).stem
                self.run_command([BINARY, "-f", examples / filename,
                                  "-x", "pdflatexsrc", "-o", stem])
                source = self.root / (stem + ".tex")
                tex = source.read_text()
                self.assertIn(r"\usepackage{tikz}", tex)
                figure = self.root / (stem + "(TpX).tpx")
                drawing = figure.read_text()
                self.assertIn(r"\begin{tikzpicture}", drawing)
                self.assertIn(formula, drawing)
                self.run_command(["pdflatex", "-halt-on-error",
                                  "-interaction=nonstopmode", source.name])
                self.assertGreater((self.root / (stem + ".pdf")).stat().st_size,
                                   1000)


if __name__ == "__main__":
    unittest.main(verbosity=2)
