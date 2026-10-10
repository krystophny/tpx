#!/usr/bin/env python3
"""Exercise the optional per-user Linux desktop registration."""
import configparser
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile
import unittest
import xml.etree.ElementTree as ET

ROOT = Path(__file__).resolve().parents[1]


@unittest.skipUnless(sys.platform.startswith("linux"),
                     "Linux desktop registration")
class LinuxDesktopEntryTests(unittest.TestCase):
    def test_installer_registers_only_native_tpx_documents_per_user(self):
        with tempfile.TemporaryDirectory(prefix="tpx portable ") as temporary:
            root = Path(temporary)
            package = root / "Portable TpX é"
            package.mkdir()
            for source, name in (
                    (ROOT / "assets/tpx.svg", "tpx.svg"),
                    (ROOT / "ci/org.tpx.TpX.desktop.in", "org.tpx.TpX.desktop.in"),
                    (ROOT / "ci/org.tpx.TpX.xml", "org.tpx.TpX.xml"),
                    (ROOT / "ci/install-linux-desktop-entry.sh",
                     "install-linux-desktop-entry.sh")):
                shutil.copy2(source, package / name)
            (package / "TpX").write_text("portable executable fixture\n")
            (package / "TpX").chmod(0o755)

            home = root / "home"
            data_home = home / ".local" / "share"
            config_home = home / ".config"
            environment = dict(os.environ, HOME=str(home),
                               XDG_DATA_HOME=str(data_home),
                               XDG_CONFIG_HOME=str(config_home))
            subprocess.run([str(package / "install-linux-desktop-entry.sh")],
                           env=environment, check=True, capture_output=True,
                           text=True, timeout=30)

            desktop = data_home / "applications/org.tpx.TpX.desktop"
            parser = configparser.ConfigParser(interpolation=None)
            parser.read(desktop, encoding="utf-8")
            entry = parser["Desktop Entry"]
            self.assertEqual(entry["Exec"], "./TpX -f %f")
            self.assertTrue(os.path.samefile(entry["Path"], package))
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


if __name__ == "__main__":
    unittest.main(verbosity=2)
