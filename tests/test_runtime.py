#!/usr/bin/env python3
"""Run native behavioral scenarios with bounded execution on Linux and macOS."""
import os
from pathlib import Path
import subprocess
import shutil
import struct
import sys
import tempfile
import uuid
import unittest

BINARY = Path(os.environ["RUNTIME_BINARY"]).resolve()
SCENARIOS = ("exit-clean", "exit-no", "exit-cancel", "exit-cancel-retry", "exit-empty-cancel", "exit-yes",
             "exit-destroy-no", "exit-destroy-yes", "exit-close",
             "exit-fallback", "exit-save-failure", "text-selection",
             "default-view", "toolbar", "draw-click", "draw-drag", "draw-rectangle", "draw-jitter", "draw-cancel", "shape-snap", "external-tools", "preview-state",
             "viewport-events", "viewport-crosshair", "viewport-preview", "viewport-damage",
             "text-metrics", "path-first-click", "async-tools",
             "document-io",
             "canvas-focus-transfer",
             "auto-reload-integration",
             "autosave-source-formats",
             "edit-session-preview-cancel", "edit-session-recovery-payload",
             "edit-session-text-whitespace", "edit-session-malformed-reference",
             "edit-session-autosave-cancel", "edit-session-recovery-disabled",
             "edit-session-deferred-reload",
             "edit-session-form-destroy",
             "property-dimensions", "font-choice", "conversion-save",
             "unsupported-exports", "live-tex-missing-tool", "clipboard-format-width",
             "clipboard-roundtrip", "color-box-custom-state",
             "platform-shortcuts", "svg-linked-image-copy", "svg-codec")


def storage_environment(home, config, state, cache):
    return {"HOME": str(home), "USERPROFILE": str(home),
            "XDG_CONFIG_HOME": str(config), "XDG_STATE_HOME": str(state),
            "XDG_CACHE_HOME": str(cache), "APPDATA": str(config),
            "LOCALAPPDATA": str(state)}


class RuntimeTests(unittest.TestCase):
    def test_transactional_pstoedit_emf_import(self):
        with tempfile.TemporaryDirectory(prefix="tpx-pstoedit-") as directory:
            root = Path(directory).resolve()
            suffix = ".exe" if sys.platform == "win32" else ""
            converter = root / ("pstoedit-fixture" + suffix)
            shutil.copy2(BINARY, converter)
            header = struct.pack(
                "<II8i4IHH3I4i", 1, 88,
                10, 20, 50, 60, 0, 0, 400, 400,
                0x464D4520, 0x10000, 132, 3, 1, 0, 0, 0, 0,
                400, 400, 100, 100)
            rectangle = struct.pack("<II4i", 43, 24, 10, 20, 50, 60)
            eof = struct.pack("<II3I", 14, 20, 0, 0, 20)
            fixture = root / "converted.emf"
            fixture.write_bytes(header + rectangle + eof)
            self.run_scenario("pstoedit-import", {
                "TPX_PSTOEDIT_PATH": str(converter),
                "TPX_PSTOEDIT_EMF_FIXTURE": str(fixture),
            })

    def test_tpx_staged_sidecars_and_converter_failures(self):
        with tempfile.TemporaryDirectory(prefix="tpx-staged-tools-") as directory:
            root = Path(directory).resolve()
            unicode_root = root / "Grüß-Καλημέρα"
            unicode_root.mkdir()
            suffix = ".exe" if sys.platform == "win32" else ""
            meta = root / ("mpost-fixture" + suffix)
            ghostscript = root / ("gs-fixture" + suffix)
            shutil.copy2(BINARY, meta)
            shutil.copy2(BINARY, ghostscript)
            marker = root / "invoked.txt"
            self.run_scenario("tpx-staged-sidecars", {
                "TPX_STAGED_META_CONVERTER": str(meta),
                "TPX_STAGED_GS_CONVERTER": str(ghostscript),
                "TPX_STAGED_TOOL_MARKER": str(marker),
                "TPX_STAGED_TEST_ROOT": str(unicode_root),
            })
            self.assertEqual(marker.read_text().splitlines(),
                             ["mpost-fixture" + suffix, "gs-fixture" + suffix])

    def test_bitmap_eps_compatibility_and_conversion_failures(self):
        # The fixture converter lives only in this test's temporary directory.
        # It runs the copied native test executable before Application.Initialize.
        with tempfile.TemporaryDirectory(prefix="tpx-eps-") as directory:
            root = Path(directory).resolve()
            unicode_root = root / "Grüß-Καλημέρα"
            unicode_root.mkdir()
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
                extensions = ((".bmp", ".png", ".jpg", ".jpeg")
                              if expected is not None else (".bmp",))
                for extension in extensions:
                    with self.subTest(case=case, extension=extension):
                        source = root / "fixture.eps"
                        input_file = unicode_root / ("source" + extension)
                        output = unicode_root / "converted.eps"
                        source.write_bytes(fixture)
                        bitmap = (b"BM" + struct.pack("<IHHI", 58, 0, 0, 54) +
                                  struct.pack("<IiiHHIIiiII", 40, 1, 1, 1, 24,
                                              0, 4, 2835, 2835, 0, 0) +
                                  b"\xff\x00\xff\x00")
                        input_file.write_bytes(bitmap)
                        # Conversion must discard stale output even when the tool fails.
                        output.write_bytes(b"stale output")
                        self.run_scenario("bitmap-eps", {
                            "TMPDIR": str(root), "TMP": str(root), "TEMP": str(root),
                            "TPX_EPS_CONVERTER": str(converter),
                            "TPX_EPS_FIXTURE_MODE": case,
                            "TPX_EPS_FIXTURE": str(source),
                            "TPX_EPS_INPUT": str(input_file),
                            "TPX_EPS_OUTPUT": str(output),
                            "TPX_EPS_EXPECT_INPUT_EXT": extension,
                            "TPX_EPS_EXPECT_SUCCESS": "1" if expected is not None else "0",
                        })
                        if expected is None:
                            self.assertFalse(output.exists())
                        else:
                            self.assertEqual(output.read_bytes(), expected)
                        self.assertFalse((root / "(bitmap2eps)eps.eps").exists(),
                                         "EPS compatibility check leaked its scratch file")

    def run_scenario(self, scenario, extra_env=None, directory_prefix="tpx-runtime-",
                     beside_executable=(), runtime_directory=None,
                     readonly_runtime=False, portable=False,
                     portable_argument=False, unicode_profile=False):
        with tempfile.TemporaryDirectory(prefix=directory_prefix) as directory:
            root = Path(directory).resolve()
            suffix = " space-Ünicode-Καλημέρα-ß" if unicode_profile else ""
            home = root / ("home" + suffix)
            config = root / ("config" + suffix)
            state = root / ("state" + suffix)
            cache = root / ("cache" + suffix)
            work = root / "work"
            home.mkdir()
            config.mkdir()
            state.mkdir()
            cache.mkdir()
            work.mkdir()
            runtime_dir = (Path(runtime_directory).resolve() if runtime_directory
                           else root / ("runtime" + suffix))
            runtime_dir.mkdir(parents=True, exist_ok=True)
            runtime_binary = runtime_dir / BINARY.name
            if not runtime_binary.exists():
                shutil.copy2(BINARY, runtime_binary)
            for resource_name in ("preview.tex.inc", "metapost.tex.inc", "help", "Help"):
                source = BINARY.parent / resource_name
                if not source.exists():
                    source = Path(__file__).resolve().parents[1] / "ci" / resource_name
                destination = runtime_dir / resource_name
                if source.is_dir() and not destination.exists():
                    shutil.copytree(source, runtime_dir / resource_name)
                elif source.is_file() and not destination.exists():
                    shutil.copy2(source, runtime_dir / resource_name)
            for file_name in beside_executable:
                (runtime_dir / file_name).touch()
            env = os.environ.copy()
            env.update({"TMPDIR": str(root), "TMP": str(root), "TEMP": str(root),
                        "HOME": str(home), "USERPROFILE": str(home),
                        "XDG_CONFIG_HOME": str(config),
                        "XDG_STATE_HOME": str(state), "XDG_CACHE_HOME": str(cache),
                        "APPDATA": str(config), "LOCALAPPDATA": str(state)})
            env.update(extra_env or {})
            expected_entries = {path.name for path in runtime_dir.iterdir()}
            readonly_modes = {}
            if readonly_runtime and os.name != "nt":
                readonly_paths = [runtime_dir, *runtime_dir.rglob("*")]
                readonly_modes = {path: path.stat().st_mode & 0o777
                                  for path in readonly_paths}
                for path in sorted(readonly_paths, key=lambda item: len(item.parts),
                                   reverse=True):
                    path.chmod(readonly_modes[path] & ~0o222)
            try:
                arguments = [str(runtime_binary), scenario]
                if portable_argument:
                    arguments.append("--portable")
                result = subprocess.run(arguments, cwd=work,
                                        env=env, capture_output=True, text=True,
                                        encoding="utf-8", errors="replace", timeout=40)
            finally:
                for path in sorted(readonly_modes, key=lambda item: len(item.parts)):
                    path.chmod(readonly_modes[path])
            self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
            self.assertIn("PASS " + scenario, result.stdout, result.stderr)
            if scenario == "storage-paths":
                values = dict(line.split("=", 1) for line in result.stdout.splitlines()
                              if line.startswith("STORAGE_"))
                self.assertEqual(values.get("STORAGE_MODE"),
                                 "portable" if portable else "installed")
                if portable:
                    storage_root = runtime_dir / "TpX-portable-data"
                    expected = {"STORAGE_CONFIG": storage_root / "config",
                                "STORAGE_STATE": storage_root / "state",
                                "STORAGE_CACHE": storage_root / "cache"}
                elif sys.platform == "darwin":
                    expected = {
                        "STORAGE_CONFIG": home / "Library" / "Preferences" / "TpX",
                        "STORAGE_STATE": home / "Library" / "Application Support" / "TpX",
                        "STORAGE_CACHE": home / "Library" / "Caches" / "TpX"}
                elif sys.platform == "win32":
                    expected = {"STORAGE_CONFIG": config / "TpX",
                                "STORAGE_STATE": state / "TpX",
                                "STORAGE_CACHE": state / "TpX"}
                else:
                    expected = {"STORAGE_CONFIG": config / "TpX",
                                "STORAGE_STATE": state / "TpX",
                                "STORAGE_CACHE": cache / "TpX"}
                for key, path in expected.items():
                    self.assertEqual(Path(values[key]).resolve(), path.resolve(), key)
                self.assertTrue((Path(values["STORAGE_STATE"]) /
                                 "AutoSaveRecovery").is_dir(),
                                "recovery store is not under the user state directory")
                expected_after = expected_entries | ({"TpX-portable-data"}
                                                      if portable else set())
                self.assertEqual({path.name for path in runtime_dir.iterdir()},
                                 expected_after,
                                 "application wrote into its executable directory")

    def test_user_storage_works_from_read_only_application_directory(self):
        self.run_scenario("storage-paths", readonly_runtime=True,
                          unicode_profile=True)

    def test_explicit_portable_storage_layout(self):
        self.run_scenario("storage-paths", beside_executable=("TpX.portable",),
                          portable=True, unicode_profile=True)

    def test_explicit_portable_flag(self):
        self.run_scenario("storage-paths", portable=True,
                          portable_argument=True, unicode_profile=True)

    def test_runtime_scenarios(self):
        for scenario in SCENARIOS:
            with self.subTest(scenario=scenario):
                self.run_scenario(scenario)

    def test_registered_tikz_open_materializes_native_scene(self):
        fixture_dir = Path(__file__).parent / "core" / "fixtures" / "tikz"
        with tempfile.TemporaryDirectory(prefix="tpx-tikz-import-") as directory:
            source = Path(directory) / "independent-scene.tex"
            fragment = Path(directory) / "bare-fragment.tikz"
            shutil.copy2(fixture_dir / source.name, source)
            shutil.copy2(fixture_dir / "acceptance-image.png", source.parent)
            fragment.write_text(
                r"\path[line width=0mm] (0,0) rectangle +(2,1);" +
                r"\draw (0,0)--(2,1);", encoding="ascii")
            self.run_scenario("tikz-import-native", {
                "TPX_TIKZ_FIXTURE": str(source),
                "TPX_TIKZ_FRAGMENT_FIXTURE": str(fragment),
            })

    def test_tikz_source_edit_save_as_and_fresh_document(self):
        fixture_dir = Path(__file__).parent / "core" / "fixtures" / "tikz"
        with tempfile.TemporaryDirectory(prefix="tpx-tikz-save-") as directory:
            root = Path(directory).resolve()
            source_dir = root / "source"
            source_dir.mkdir()
            source = source_dir / "independent-scene.tex"
            shutil.copy2(fixture_dir / source.name, source)
            shutil.copy2(fixture_dir / "acceptance-image.png", source_dir)
            relocated_dir = root / "relocated"
            relocated_dir.mkdir()
            fresh_dir = root / "fresh"
            fresh_dir.mkdir()
            rejected = fresh_dir / "rejected.tikz"
            rejected.write_bytes(b"preserve unsupported Save As target")
            self.run_scenario("tikz-source-save", {
                "TPX_TIKZ_SOURCE_FILE": str(source),
                "TPX_TIKZ_RELOCATED_FILE": str(relocated_dir / source.name),
                "TPX_TIKZ_FRESH_FILE": str(fresh_dir / "fractional.tikz"),
                "TPX_TIKZ_REJECTED_FILE": str(rejected),
                "TPX_TIKZ_FRESH_TEX_FILE": str(fresh_dir / "standalone.tex"),
                "TPX_TIKZ_EMPTY_FILE": str(fresh_dir / "empty.tikz"),
            })

    @unittest.skipUnless(shutil.which("pdflatex") and shutil.which("gs"),
                         "pdflatex and Ghostscript are required to render copied TikZ")
    def test_copy_as_tikz_compiles_and_renders_without_tex_dependency(self):
        with tempfile.TemporaryDirectory(prefix="tpx-copy-tikz-") as directory:
            root = Path(directory).resolve()
            fragment = root / "clipboard.tikz"
            self.run_scenario("copy-as-tikz", {
                "TPX_COPY_TIKZ_OUTPUT": str(fragment),
                # The application command must be usable without TeX on PATH.
                "PATH": "",
            })
            source = fragment.read_text(encoding="utf-8")
            self.assertIn(r"\usepackage{tikz}", source)
            self.assertIn("1.125mm", source)
            self.assertIn("2.375mm", source)
            self.assertIn("rotate=", source)
            self.assertIn("$x^2$", source)
            self.assertIn("β", source)

            (root / "copy.tex").write_text(
                r"""\documentclass{article}
\usepackage[utf8]{inputenc}
\usepackage{tikz}
\DeclareUnicodeCharacter{03B2}{\ensuremath{\beta}}
\pagestyle{empty}
\begin{document}
""" + source + r"""\end{document}
""", encoding="utf-8")
            compiled = subprocess.run(
                [shutil.which("pdflatex"), "-halt-on-error", "-interaction=nonstopmode", "copy.tex"],
                cwd=root, capture_output=True, text=True, timeout=40)
            self.assertEqual(compiled.returncode, 0,
                             compiled.stdout[-4000:] + compiled.stderr[-2000:])
            image = root / "copy.ppm"
            rendered = subprocess.run(
                [shutil.which("gs"), "-q", "-dSAFER", "-dBATCH", "-dNOPAUSE",
                 "-sDEVICE=ppmraw", "-r72", "-sOutputFile=" + str(image), "copy.pdf"],
                cwd=root, capture_output=True, text=True, timeout=40)
            self.assertEqual(rendered.returncode, 0,
                             rendered.stdout[-2000:] + rendered.stderr[-2000:])
            data = image.read_bytes()
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
            blue_pixels = sum(
                rgb[index] < 10 and rgb[index + 1] < 10 and rgb[index + 2] > 240
                for index in range(0, len(rgb), 3))
            self.assertGreater(blue_pixels, 100,
                               "Rendered copied TikZ did not contain the filled rectangle")

    def test_untitled_recovery_survives_restart_and_requires_choice(self):
        with tempfile.TemporaryDirectory(prefix="tpx-autosave-restart-") as directory:
            root = Path(directory).resolve()
            # The recovery bundle and bitmap conversion stage live under the
            # profile path. Exercise UTF-8 process launch there and GetTempPath.
            home = root / "home-Καλημέρα-ß"
            config = root / "config-Καλημέρα-ß"
            state = root / "state-Καλημέρα-ß"
            cache = root / "cache-Καλημέρα-ß"
            temp = root / "temp-Καλημέρα-ß"
            home.mkdir()
            config.mkdir()
            state.mkdir()
            cache.mkdir()
            temp.mkdir()
            env = storage_environment(home, config, state, cache)
            env.update(TMPDIR=str(temp), TMP=str(temp), TEMP=str(temp))
            first_runtime_dir = root / "runtime-before-move-Καλημέρα-ß"
            moved_runtime_dir = root / "runtime-after-move-Καλημέρα-ß"
            self.run_scenario("autosave-recovery-create", env,
                              runtime_directory=first_runtime_dir)
            self.run_scenario("autosave-recovery-restore", env,
                              runtime_directory=moved_runtime_dir)

    def test_user_preferences_survive_executable_replacement(self):
        with tempfile.TemporaryDirectory(prefix="tpx-settings-restart-") as directory:
            root = Path(directory).resolve()
            home = root / "home"
            config = root / "config"
            state = root / "state"
            cache = root / "cache"
            home.mkdir()
            config.mkdir()
            state.mkdir()
            cache.mkdir()
            env = storage_environment(home, config, state, cache)
            first_runtime_dir = root / "runtime-before-replacement-Καλημέρα-ß"
            replacement_runtime_dir = root / "runtime-after-replacement-Καλημέρα-ß"
            self.run_scenario("user-preferences-write", env,
                              runtime_directory=first_runtime_dir)
            self.run_scenario("user-preferences-read", env,
                              runtime_directory=replacement_runtime_dir)

    def test_dirty_empty_draft_survives_restart_without_source_write(self):
        with tempfile.TemporaryDirectory(prefix="tpx-autosave-empty-restart-") as directory:
            root = Path(directory).resolve()
            home = root / "home"
            config = root / "config"
            state = root / "state"
            cache = root / "cache"
            home.mkdir()
            config.mkdir()
            state.mkdir()
            cache.mkdir()
            env = storage_environment(home, config, state, cache)
            env["TPX_AUTOSAVE_EMPTY_SOURCE"] = str(root / "empty.tpx")
            runtime_dir = root / "runtime"
            self.run_scenario("autosave-empty-recovery-create", env,
                              runtime_directory=runtime_dir)
            self.run_scenario("autosave-empty-recovery-restore", env,
                              runtime_directory=runtime_dir)

    def test_recovery_warns_about_unresolved_bitmap_links(self):
        with tempfile.TemporaryDirectory(prefix="tpx-autosave-warning-") as directory:
            root = Path(directory).resolve()
            home = root / "home"
            config = root / "config"
            state = root / "state"
            cache = root / "cache"
            home.mkdir()
            config.mkdir()
            state.mkdir()
            cache.mkdir()
            env = storage_environment(home, config, state, cache)
            self.run_scenario("autosave-unresolved-image-warning", env)

    def test_named_bitmap_recovery_gates_source_autosave_until_manual_save(self):
        with tempfile.TemporaryDirectory(prefix="tpx-autosave-named-restart-") as directory:
            root = Path(directory).resolve()
            home = root / "home"
            config = root / "config"
            state = root / "state"
            cache = root / "cache"
            home.mkdir()
            config.mkdir()
            state.mkdir()
            cache.mkdir()
            env = storage_environment(home, config, state, cache)
            env["TPX_AUTOSAVE_NAMED_SOURCE"] = str(root / "named.tpx")
            runtime_dir = root / "runtime"
            self.run_scenario("autosave-named-recovery-create", env,
                              runtime_directory=runtime_dir)
            self.run_scenario("autosave-named-recovery-restore", env,
                              runtime_directory=runtime_dir)

    def test_recovery_retires_old_source_watch_only_after_commit(self):
        with tempfile.TemporaryDirectory(prefix="tpx-autosave-watch-restore-") as directory:
            root = Path(directory).resolve()
            home = root / "home"
            config = root / "config"
            state = root / "state"
            cache = root / "cache"
            home.mkdir()
            config.mkdir()
            state.mkdir()
            cache.mkdir()
            env = storage_environment(home, config, state, cache)
            env["TPX_AUTOSAVE_WATCH_SOURCE"] = str(root / "active.tpx")
            self.run_scenario("autosave-recovery-watch-binding", env)

    def test_source_autosave_undo_picture_properties_and_conflict(self):
        with tempfile.TemporaryDirectory(prefix="tpx-autosave-source-") as directory:
            root = Path(directory).resolve()
            home = root / "home"
            config = root / "config"
            state = root / "state"
            cache = root / "cache"
            home.mkdir()
            config.mkdir()
            state.mkdir()
            cache.mkdir()
            env = storage_environment(home, config, state, cache)
            self.run_scenario("autosave-integration", env)

    @unittest.skipUnless(shutil.which("latex") and
                         (shutil.which("dvipng") or
                          (shutil.which("dvisvgm") and shutil.which("rsvg-convert"))),
                         "real LaTeX and a preview rasterizer are required")
    def test_live_latex_batch_cache_edits_and_failure(self):
        self.run_scenario("live-tex")

    @unittest.skipIf(sys.platform == "darwin", "covered by macOS settings test")
    def test_live_latex_preference_persists(self):
        self.run_scenario("live-tex-settings")

    @unittest.skipUnless(sys.platform.startswith("linux"),
                         "native editable shortcut routing uses GTK2/X11")
    def test_gtk_editable_shortcut_routing(self):
        self.run_scenario("editable-shortcut-routing")

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
                    env.update(storage_environment(
                        root / "home", root / "config", root / "state",
                        root / "cache"))
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
        with tempfile.TemporaryDirectory(prefix="tpx-empty-path-") as directory:
            self.run_scenario("check-file-path", {
                "PATH": directory,
                "TPX_FILEPATH_TOOL": tool_name,
            }, beside_executable=(tool_name,))

    @unittest.skipUnless(sys.platform == "darwin", "macOS user settings location")
    def test_macos_settings_are_saved_outside_application_bundle(self):
        for legacy_layout in ("none", "old-user-config", "bundle"):
            with self.subTest(legacy_layout=legacy_layout), \
                    tempfile.TemporaryDirectory(prefix="tpx-mac-settings-") as directory:
                root = Path(directory).resolve()
                bundle_dir = root / "TpX.app" / "Contents" / "MacOS"
                bundle_dir.mkdir(parents=True)
                executable = bundle_dir / "RuntimeTests"
                shutil.copy2(BINARY, executable)
                bundle_ini = bundle_dir / "TpX.ini"
                original_legacy = "LineWidth_Default=2.25\n"
                if legacy_layout == "bundle":
                    bundle_ini.write_text(original_legacy)

                home = root / "home"
                config_home = root / "config"
                home.mkdir()
                config_home.mkdir()
                state_home = root / "state"
                cache_home = root / "cache"
                state_home.mkdir()
                cache_home.mkdir()
                env = os.environ.copy()
                env.update(storage_environment(home, config_home,
                                               state_home, cache_home),
                           TMPDIR=str(root))
                old_user_settings = None
                if legacy_layout == "old-user-config":
                    seed = subprocess.run(
                        [str(executable), "seed-legacy-mac-settings"], cwd=root,
                        env=env, capture_output=True, text=True, timeout=40)
                    self.assertEqual(seed.returncode, 0,
                                     seed.stdout + seed.stderr)
                    legacy_line = next(line for line in seed.stdout.splitlines()
                                       if line.startswith("LEGACY_SETTINGS_FILE="))
                    old_user_settings = Path(
                        legacy_line.split("=", 1)[1]).resolve()
                    self.assertTrue(old_user_settings.is_file())
                    self.assertEqual(old_user_settings.read_text(), original_legacy)
                if legacy_layout != "none":
                    env["TPX_SETTINGS_EXPECT_LEGACY"] = "2.25"
                else:
                    env.pop("TPX_SETTINGS_EXPECT_LEGACY", None)
                result = subprocess.run([str(executable), "mac-settings"], cwd=root,
                                        env=env, capture_output=True, text=True,
                                        timeout=40)
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                self.assertIn("PASS mac-settings", result.stdout, result.stderr)
                config_line = next(line for line in result.stdout.splitlines()
                                   if line.startswith("SETTINGS_FILE="))
                settings_file = Path(config_line.split("=", 1)[1]).resolve()
                self.assertTrue(settings_file.is_file())
                self.assertNotEqual(settings_file, bundle_ini.resolve())
                self.assertTrue(settings_file.is_relative_to(root))
                legacy_file = (bundle_ini if legacy_layout == "bundle"
                               else old_user_settings)
                if legacy_file is not None:
                    self.assertEqual(legacy_file.read_text(), original_legacy,
                                     "Legacy preferences were modified")
                else:
                    self.assertFalse(bundle_ini.exists(),
                                     "Saving created TpX.ini inside the app bundle")

    @unittest.skipUnless(sys.platform == "darwin", "macOS bundle resources")
    def test_macos_resources_and_editable_templates(self):
        for layout in ("bundle", "legacy-bundle", "bare"):
            with self.subTest(layout=layout), \
                    tempfile.TemporaryDirectory(prefix="tpx-mac-resources-") as directory:
                root = Path(directory).resolve()
                bundled = layout == "bundle"
                app_bundle = layout != "bare"
                if app_bundle:
                    executable_dir = root / "TpX.app" / "Contents" / "MacOS"
                    resource_dir = root / "TpX.app" / "Contents" / "Resources"
                    resource_dir.mkdir(parents=True)
                    (resource_dir.parent / "Info.plist").write_text("test app bundle\n")
                else:
                    executable_dir = root / "bare-bin"
                    resource_dir = root / "unused-resources"
                template_dir = resource_dir if bundled else executable_dir
                help_source_dir = resource_dir if bundled else executable_dir
                help_dir = resource_dir / "help"
                executable_dir.mkdir(parents=True)
                if bundled:
                    help_dir.mkdir(parents=True)
                else:
                    (help_source_dir / "help").mkdir(parents=True)
                executable = executable_dir / "RuntimeTests"
                shutil.copy2(BINARY, executable)
                resources = {
                    "preview.tex.inc": b"% packaged preview default\n",
                    "metapost.tex.inc": b"% packaged MetaPost default\n",
                    "help/tpx_tpxabout_tpx_drawing_tool.htm":
                        b"<html>packaged TpX help</html>\n",
                }
                for name, contents in resources.items():
                    source_dir = help_source_dir if name.startswith("help/") else template_dir
                    (source_dir / name).write_bytes(contents)

                home = root / "home"
                config_home = root / "config"
                state_home = root / "state"
                cache_home = root / "cache"
                home.mkdir()
                config_home.mkdir()
                state_home.mkdir()
                cache_home.mkdir()
                env = os.environ.copy()
                env.update(storage_environment(home, config_home,
                                               state_home, cache_home),
                           TMPDIR=str(root),
                           TPX_RESOURCE_EXPECT_BUNDLE="1" if bundled else "0")
                result = subprocess.run([str(executable), "mac-resources"], cwd=root,
                                        env=env, capture_output=True, text=True,
                                        timeout=40)
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                self.assertIn("PASS mac-resources", result.stdout, result.stderr)
                for name, original in resources.items():
                    source_dir = help_source_dir if name.startswith("help/") else template_dir
                    self.assertEqual((source_dir / name).read_bytes(), original,
                                     f"Packaged resource changed: {name}")
                template_paths = [line.split("=", 2)[2]
                                  for line in result.stdout.splitlines()
                                  if line.startswith("TEMPLATE_PATH=")]
                self.assertEqual(len(template_paths), 2, result.stdout)
                for name, path in zip(("preview.tex.inc", "metapost.tex.inc"),
                                      template_paths):
                    user_template = Path(path).resolve()
                    self.assertTrue(user_template.is_file())
                    self.assertTrue(user_template.is_relative_to(root))
                    self.assertNotEqual(user_template, (template_dir / name).resolve())
                    self.assertIn("% user customization", user_template.read_text())
                help_path = next(line.split("=", 1)[1]
                                 for line in result.stdout.splitlines()
                                 if line.startswith("HELP_PATH="))
                expected_help = help_source_dir / "help/tpx_tpxabout_tpx_drawing_tool.htm"
                self.assertEqual(Path(help_path).resolve(), expected_help.resolve())


if __name__ == "__main__":
    unittest.main()
