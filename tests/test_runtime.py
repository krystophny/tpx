#!/usr/bin/env python3
"""Run native behavioral scenarios with bounded execution on Linux and macOS."""
import os
from pathlib import Path
import subprocess
import shutil
import sys
import tempfile
import unittest

BINARY = Path(os.environ["RUNTIME_BINARY"]).resolve()
SCENARIOS = ("exit-clean", "exit-no", "exit-cancel", "exit-yes",
             "exit-close", "exit-fallback", "text-selection", "default-view", "toolbar", "draw-click", "draw-drag", "draw-rectangle", "draw-jitter", "draw-cancel", "shape-snap", "external-tools", "preview-state",
             "viewport-events", "viewport-crosshair", "viewport-preview",
             "text-metrics", "path-first-click", "async-tools",
             "property-dimensions", "font-choice", "conversion-save",
             "unsupported-exports")


class RuntimeTests(unittest.TestCase):
    def run_scenario(self, scenario, extra_env=None, directory_prefix="tpx-runtime-"):
        with tempfile.TemporaryDirectory(prefix=directory_prefix) as directory:
            env = os.environ.copy()
            env["TMPDIR"] = directory
            env.update(extra_env or {})
            result = subprocess.run([str(BINARY), scenario], cwd=directory,
                                    env=env, capture_output=True, text=True, timeout=40)
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertIn("PASS " + scenario, result.stdout, result.stderr)

    def test_runtime_scenarios(self):
        for scenario in SCENARIOS:
            with self.subTest(scenario=scenario):
                self.run_scenario(scenario)

    @unittest.skipUnless(shutil.which("pdflatex"), "pdflatex is not installed")
    def test_standalone_labeled_previews(self):
        self.run_scenario("labeled-preview")

    @unittest.skipUnless(sys.platform.startswith("linux") and shutil.which("pdflatex"),
                         "Linux temp-directory override and pdflatex are required")
    def test_preview_in_special_temp_directory(self):
        # Exercise the real preview compiler with a TeX-active parent path.
        self.run_scenario("labeled-preview", directory_prefix="tpx ~preview-")

    @unittest.skipUnless(sys.platform.startswith("linux"), "Linux desktop opener")
    def test_default_document_opener(self):
        with tempfile.TemporaryDirectory(prefix="tpx-opener-") as directory:
            root = Path(directory)
            opener = root / "xdg-open"
            opener.write_text('#!/bin/sh\nprintf "%s\\n" "$1" > "$TPX_OPENER_LOG"\n')
            opener.chmod(0o755)
            self.run_scenario("default-opener", {
                "PATH": directory + os.pathsep + os.environ["PATH"],
                "TPX_OPENER_LOG": str(root / "opened"),
            })


if __name__ == "__main__":
    unittest.main()
