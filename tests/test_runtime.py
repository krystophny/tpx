#!/usr/bin/env python3
"""Run native behavioral scenarios with bounded execution on Linux and macOS."""
import os
from pathlib import Path
import subprocess
import shutil
import sys
import tempfile
import uuid
import unittest

BINARY = Path(os.environ["RUNTIME_BINARY"]).resolve()
SCENARIOS = ("exit-clean", "exit-no", "exit-cancel", "exit-yes",
             "exit-close", "exit-fallback", "text-selection", "default-view", "toolbar", "draw-click", "draw-drag", "draw-rectangle", "draw-jitter", "draw-cancel", "shape-snap", "external-tools", "preview-state",
             "viewport-events", "viewport-crosshair", "viewport-preview",
             "text-metrics", "path-first-click", "async-tools",
             "property-dimensions", "font-choice", "conversion-save",
             "unsupported-exports")


class RuntimeTests(unittest.TestCase):
    def test_bitmap_eps_compatibility_and_conversion_failures(self):
        # The fixture converter lives only in this test's temporary directory.
        # It runs the copied native test executable before Application.Initialize.
        with tempfile.TemporaryDirectory(prefix="tpx-eps-") as directory:
            root = Path(directory).resolve()
            suffix = ".exe" if sys.platform == "win32" else ""
            converter = root / ("sam2p-fixture" + suffix)
            shutil.copy2(BINARY, converter)
            modern = (b"%!PS-Adobe-3.0 EPSF-3.0\r\n"
                      b"%%BoundingBox: 0 0 64 64\r\n"
                      b"1 0 0 setrgbcolor\r\n0 0 64 64 rectfill\r\nshowpage\r\n")
            legacy = os.linesep.join(("%!PS-Adobe-3.0 EPSF-3.0",
                       "%%BoundingBox: 0 0 64 64", "%%BeginData: 0 Binary Bytes",
                       "1 0 0 setrgbcolor", "0 0 64 64 rectfill", "showpage", "")).encode()
            for case, fixture, expected in (
                    ("modern", modern, modern),
                    ("legacy", legacy, legacy.replace(b"%%BeginData:", b"%%BeginData")),
                    ("no-output", modern, None), ("nonzero", modern, None)):
                with self.subTest(case=case):
                    source = root / "fixture.eps"
                    output = root / "converted.eps"
                    source.write_bytes(fixture)
                    # Conversion must discard stale output even when the tool fails.
                    output.write_bytes(b"stale output")
                    self.run_scenario("bitmap-eps", {
                        "TMPDIR": str(root), "TMP": str(root), "TEMP": str(root),
                        "TPX_EPS_CONVERTER": str(converter),
                        "TPX_EPS_FIXTURE_MODE": case,
                        "TPX_EPS_FIXTURE": str(source),
                        "TPX_EPS_INPUT": str(root / "source.bmp"),
                        "TPX_EPS_OUTPUT": str(output),
                        "TPX_EPS_EXPECT_SUCCESS": "1" if expected is not None else "0",
                    })
                    if expected is None:
                        self.assertFalse(output.exists())
                    else:
                        self.assertEqual(output.read_bytes(), expected)
                    self.assertFalse((root / "(bitmap2eps)eps.eps").exists(),
                                     "EPS compatibility check leaked its scratch file")

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

    def test_startup_document_identity(self):
        # Keep scenario selection out of argv so production startup sees real input.
        with tempfile.TemporaryDirectory(prefix="tpx startup-") as directory:
            root = Path(directory).resolve()
            source = root / "drawing with spaces.tpx"
            source.write_text('%<TpX v="5">\n'
                              '%<line x1="0" y1="0" x2="20" y2="10"/>\n'
                              '%</TpX>\n')
            cases = [([], ": Unnamed drawing :", 0),
                     (["-f", ""], ": Unnamed drawing :", 0),
                     (["new drawing.tpx"], str(root / "new drawing.tpx"), 0),
                     (["-f", source.name], str(source), 1)]
            for arguments, expected, count in cases:
                with self.subTest(arguments=arguments):
                    env = os.environ.copy()
                    env.update(TPX_STARTUP_EXPECTED=expected,
                               TPX_STARTUP_OBJECTS=str(count))
                    result = subprocess.run([str(BINARY), *arguments], cwd=root,
                                            env=env, capture_output=True,
                                            text=True, timeout=40)
                    self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                    self.assertIn("PASS startup-file", result.stdout, result.stderr)

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

    def test_check_file_path_uses_platform_path_separator(self):
        with tempfile.TemporaryDirectory(prefix="tpx-path-tool-") as directory:
            tool_name = "tpx-path-probe-" + uuid.uuid4().hex
            tool = Path(directory) / tool_name
            tool.write_bytes(b"")
            self.run_scenario("check-file-path", {
                "PATH": directory,
                "TPX_FILEPATH_TOOL": tool_name,
            })

    def test_check_file_path_searches_beside_runtime_executable(self):
        tool_name = "tpx-path-probe-" + uuid.uuid4().hex
        tool = BINARY.parent / tool_name
        with tempfile.TemporaryDirectory(prefix="tpx-empty-path-") as directory:
            try:
                tool.write_bytes(b"")
                self.run_scenario("check-file-path", {
                    "PATH": directory,
                    "TPX_FILEPATH_TOOL": tool_name,
                })
            finally:
                tool.unlink(missing_ok=True)


if __name__ == "__main__":
    unittest.main()
