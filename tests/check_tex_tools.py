#!/usr/bin/env python3
"""Fail the TeX test target when its render tools are missing."""
import shutil
import sys


missing = ["pdflatex"] if not shutil.which("pdflatex") else []
if missing:
    print("Mandatory TeX test prerequisite missing: " + ", ".join(missing),
          file=sys.stderr)
    raise SystemExit(1)
print("Mandatory TeX test prerequisite: pdflatex (PASS)")
print("Optional Ghostscript/dvips/sam2p test paths retain their explicit skips.")
