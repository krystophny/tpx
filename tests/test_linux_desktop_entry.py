#!/usr/bin/env python3
"""Exercise the optional per-user Linux desktop registration."""
import configparser
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import time
import unittest
import xml.etree.ElementTree as ET

import headless_display

headless_display.ensure_private_display()

ROOT = Path(__file__).resolve().parents[1]


@unittest.skipUnless(sys.platform.startswith("linux"),
                     "Linux desktop registration")
class LinuxDesktopEntryTests(unittest.TestCase):
    @unittest.skipUnless(shutil.which("gio"), "GIO launcher is required")
    def test_gio_launches_only_native_tpx_documents_from_any_working_directory(self):
        with tempfile.TemporaryDirectory(prefix="tpx portable ") as temporary:
            root = Path(temporary)
            package = root / 'Portable TpX % "quoted" \\ $ `ticks` é'
            package.mkdir()
            for source, name in (
                    (ROOT / "assets/tpx.svg", "tpx.svg"),
                    (ROOT / "ci/org.tpx.TpX.desktop.in", "org.tpx.TpX.desktop.in"),
                    (ROOT / "ci/org.tpx.TpX.xml", "org.tpx.TpX.xml"),
                    (ROOT / "ci/install-linux-desktop-entry.sh",
                     "install-linux-desktop-entry.sh")):
                shutil.copy2(source, package / name)
            marker = root / "launch.json"
            fixture = "#!/usr/bin/env python3\nimport json, os, sys\n" \
                "with open(os.environ['TPX_LAUNCH_MARKER'], 'w', encoding='utf-8') as f:\n" \
                "    json.dump({'argv': sys.argv[1:], 'cwd': os.getcwd()}, f)\n"
            (package / "TpX").write_text(fixture, encoding="utf-8")
            (package / "TpX").chmod(0o755)

            home = root / "home"
            data_home = home / ".local" / "share"
            config_home = home / ".config"
            environment = dict(os.environ, HOME=str(home),
                               XDG_DATA_HOME=str(data_home),
                               XDG_CONFIG_HOME=str(config_home),
                               TPX_LAUNCH_MARKER=str(marker))
            subprocess.run([str(package / "install-linux-desktop-entry.sh")],
                           env=environment, check=True, capture_output=True,
                           text=True, timeout=30)

            desktop = data_home / "applications/org.tpx.TpX.desktop"
            parser = configparser.ConfigParser(interpolation=None)
            parser.read(desktop, encoding="utf-8")
            entry = parser["Desktop Entry"]
            self.assertTrue(entry["Exec"].startswith('"/usr/bin/env" "'))
            self.assertTrue(entry["Exec"].endswith('" -f %f'))
            self.assertEqual(entry["Path"].replace("\\\\", "\\"), str(package))
            self.assertEqual(entry["MimeType"], "application/x-tpx;")
            self.assertEqual(entry["Icon"], "org.tpx.TpX")
            self.assertEqual((data_home / "icons/hicolor/scalable/apps/"
                              "org.tpx.TpX.svg").read_bytes(),
                             (ROOT / "assets/tpx.svg").read_bytes())

            mime = ET.parse(data_home / "mime/packages/org.tpx.TpX.xml")
            namespace = {"mime": "http://www.freedesktop.org/standards/shared-mime-info"}
            types = mime.findall("mime:mime-type", namespace)
            self.assertEqual([item.get("type") for item in types],
                             ["application/x-tpx"])
            globs = types[0].findall("mime:glob", namespace)
            self.assertEqual([item.get("pattern") for item in globs], ["*.tpx"])

            drawing = root / "Dessin 100% – β.tpx"
            drawing.write_text("native drawing", encoding="utf-8")
            unrelated = root / "unrelated cwd"
            unrelated.mkdir()
            result = subprocess.run(["gio", "launch", str(desktop), str(drawing)],
                                    env=environment, cwd=unrelated,
                                    capture_output=True, text=True, timeout=30)
            self.assertEqual(result.returncode, 0,
                             desktop.read_text(encoding="utf-8") +
                             result.stdout + result.stderr)
            deadline = time.monotonic() + 10
            while not marker.exists() and time.monotonic() < deadline:
                time.sleep(0.05)
            self.assertTrue(marker.exists(), "GIO did not execute the desktop entry")
            launch = json.loads(marker.read_text(encoding="utf-8"))
            self.assertEqual(launch["argv"], ["-f", str(drawing)])
            self.assertTrue(os.path.samefile(launch["cwd"], package))


if __name__ == "__main__":
    unittest.main(verbosity=2)
