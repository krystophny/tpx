#!/usr/bin/env python3
"""Compile actual exports and inspect TeX's evaluated dimensions."""
import os
from pathlib import Path
import re
import shutil
import subprocess
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
                             TeXFigure="none", PicScale="1", Border="2", **attributes)
        ET.SubElement(drawing, "line", x1="0", y1="0", x2="20", y2="10", lw="2")
        ET.SubElement(drawing, "line", x1="0", y1="10", x2="20", y2="0", li="dash")
        ET.SubElement(drawing, "line", x1="0", y1="5", x2="20", y2="5", li="dot")
        ET.SubElement(drawing, "text", x="0", y="15", h="5", t="$(a+b)$")
        ET.SubElement(drawing, "text", x="0", y="25", h="10", t="Price $5 & tax")
        ET.SubElement(drawing, "text", x="0", y="35", h="5", t="Plain", tex=r"$x^2$")
        source = self.root / "input.TpX"
        source.write_text("%" + ET.tostring(drawing, encoding="unicode") + "\n")
        return source

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
