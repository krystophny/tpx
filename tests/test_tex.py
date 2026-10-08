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

BINARY = Path(os.environ["TPX_BINARY"]).resolve()


@unittest.skipUnless(shutil.which("pdflatex"), "pdflatex is not installed")
class TeXTests(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory(prefix="tpx-tex-")
        self.root = Path(self.directory.name)

    def tearDown(self):
        self.directory.cleanup()

    def run_command(self, command):
        result = subprocess.run(list(map(str, command)), cwd=self.root,
                                capture_output=True, text=True, timeout=40)
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
        # The copied application reads this test-only configuration beside itself.
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


if __name__ == "__main__":
    unittest.main(verbosity=2)
