#!/usr/bin/env python3
"""Exercise the actual TpX executable against independently defined drawings."""
import os
from pathlib import Path
import subprocess
import tempfile
import unittest
import xml.etree.ElementTree as ET

BINARY = Path(os.environ["TPX_BINARY"]).resolve()
SVG = "{http://www.w3.org/2000/svg}"


class ExportTests(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory(prefix="tpx-test-")
        self.root = Path(self.directory.name)

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

    def run_tpx(self, *arguments):
        result = subprocess.run([str(BINARY), *map(str, arguments)],
                                cwd=self.root, capture_output=True,
                                text=True, timeout=20)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

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


if __name__ == "__main__":
    unittest.main(verbosity=2)
