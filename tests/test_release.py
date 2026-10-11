#!/usr/bin/env python3
"""Exercise release policy and reject bad cross-platform release artifacts."""
import hashlib
import io
import json
import os
import plistlib
from pathlib import Path
import subprocess
import sys
import tarfile
import tempfile
import unittest
import zipfile

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT))
from ci import build_candidate, publish_release, release_policy, verify_release


class ReleasePolicyTests(unittest.TestCase):
    def test_branch_version_bootstrap_and_transition_matrix(self):
        ancestry = lambda base, commit: base == "v1.6.0" and commit == "source"
        self.assertEqual(release_policy.validate_release(
            "refs/heads/release/1.6", "1.6.1", "1.6.1", {"v1.6.0"},
            source_sha="source", is_ancestor=ancestry),
            {"tag": "v1.6.1", "prerelease": False})
        first = release_policy.validate_release(
            "refs/heads/release/2.0", "2.0.0-beta.1", "2.0.0-beta.1", set())
        self.assertEqual(first, {"tag": "v2.0.0-beta.1", "prerelease": True})
        tags = {"v2.0.0-beta.1"}
        release_policy.validate_release("refs/heads/release/2.0", "2.0.0-beta.2",
                                        "2.0.0-beta.2", tags)
        release_policy.validate_release("refs/heads/release/2.0", "2.0.0-rc.1",
                                        "2.0.0-rc.1", tags,
                                        published_tags={"v2.0.0-beta.1"})
        release_policy.validate_release("refs/heads/release/2.0", "2.0.0",
                                        "2.0.0", tags | {"v2.0.0-rc.1"},
                                        published_tags={"v2.0.0-beta.1", "v2.0.0-rc.1"})

    def test_rejects_wrong_branch_version_source_tag_and_bootstrap(self):
        cases = [
            ("refs/heads/master", "1.6.1", "1.6.1", {"v1.6.0"}),
            ("refs/heads/release/1.6", "2.0.0-beta.1", "2.0.0-beta.1", {"v1.6.0"}),
            ("refs/heads/release/2.0", "1.6.1", "1.6.1", set()),
            ("refs/heads/release/2.0", "2.0.0-rc.1", "2.0.0-rc.1", set()),
            ("refs/heads/release/2.0", "2.0.0-beta.1", "2.0.0", set()),
            ("refs/heads/release/1.6", "1.6.1", "1.6.1", {"v1.6.0", "v1.6.1"}),
        ]
        for ref, version, about, tags in cases:
            with self.subTest(ref=ref, version=version, tags=tags):
                with self.assertRaises(ValueError):
                    release_policy.validate_release(ref, version, about, tags)
        with self.assertRaises(ValueError):
            release_policy.validate_release("refs/heads/release/1.6", "1.6.1",
                                            "1.6.1", {"v1.6.0"}, source_sha="source",
                                            is_ancestor=lambda *_: False)
        with self.assertRaisesRegex(ValueError, "selective single-parent"):
            release_policy.validate_release("refs/heads/release/1.6", "1.6.1",
                                            "1.6.1", {"v1.6.0"}, source_sha="source",
                                            is_ancestor=lambda *_: True,
                                            is_linear=lambda *_: False)
        with self.assertRaisesRegex(ValueError, "latest beta to be published"):
            release_policy.validate_release("refs/heads/release/2.0", "2.0.0-rc.1",
                                            "2.0.0-rc.1", {"v2.0.0-beta.1"})
        with self.assertRaisesRegex(ValueError, "advance in order"):
            release_policy.validate_release("refs/heads/release/2.0", "2.0.0-rc.2",
                                            "2.0.0-rc.2", {"v2.0.0-beta.1"},
                                            published_tags={"v2.0.0-beta.1"})
        with self.assertRaisesRegex(ValueError, "published release candidate"):
            release_policy.validate_release("refs/heads/release/2.0", "2.0.0",
                                            "2.0.0", {"v2.0.0-beta.1", "v2.0.0-rc.1"},
                                            published_tags={"v2.0.0-beta.1"})

    def test_selective_backport_keeps_2_0_feature_out_of_1_6_history(self):
        with tempfile.TemporaryDirectory(prefix="tpx-backport-test-") as temporary:
            root = Path(temporary)
            def git(*args):
                return subprocess.check_output(["git", "-C", str(root), *args],
                                                text=True, stderr=subprocess.DEVNULL).strip()
            subprocess.run(["git", "init", "-q", str(root)], check=True)
            git("config", "user.name", "Release test")
            git("config", "user.email", "release-test@example.invalid")
            (root / "base.txt").write_text("1.6 base\n")
            git("add", ".")
            git("commit", "-qm", "base 1.6.0")
            base = git("rev-parse", "HEAD")
            git("switch", "-c", "integration-2.0")
            (root / "feature.txt").write_text("2.0 feature\n")
            git("add", ".")
            git("commit", "-qm", "2.0 feature")
            (root / "bugfix.txt").write_text("reviewed fix\n")
            git("add", ".")
            git("commit", "-qm", "reviewed bug fix")
            fix = git("rev-parse", "HEAD")
            git("switch", "-c", "release/1.6", base)
            git("cherry-pick", fix)
            head = git("rev-parse", "HEAD")
            self.assertTrue(release_policy.linear_history(base, head, cwd=root))
            self.assertTrue((root / "bugfix.txt").exists())
            self.assertFalse((root / "feature.txt").exists())
            git("merge", "--no-ff", "integration-2.0", "-m", "merge 2.0 features")
            self.assertFalse(release_policy.linear_history(
                base, git("rev-parse", "HEAD"), cwd=root))

    def test_only_newest_stable_release_is_marked_latest(self):
        releases = [[{"tag_name": "v2.0.0", "draft": False, "prerelease": False},
                     {"tag_name": "v2.0.0-rc.1", "draft": False, "prerelease": True}]]
        self.assertFalse(release_policy.should_mark_latest("2.0.0-beta.2", releases))
        self.assertFalse(release_policy.should_mark_latest("1.6.2", releases))
        self.assertTrue(release_policy.should_mark_latest("2.0.0", releases))


class ChecksumTests(unittest.TestCase):
    def test_sidecar_has_unix_line_endings_on_every_builder(self):
        with tempfile.TemporaryDirectory(prefix="tpx-sidecar-test-") as temporary:
            archive = Path(temporary) / "tpx-2.0.0-beta.1-windows-x86_64.zip"
            archive.write_bytes(b"archive bytes")
            digest = build_candidate.write_checksum(archive)
            sidecar = archive.with_name(archive.name + ".sha256").read_bytes()
            self.assertEqual(sidecar, f"{digest}  {archive.name}\n".encode("ascii"))
            self.assertNotIn(b"\r", sidecar)


class PackageMetadataTests(unittest.TestCase):
    def test_prerelease_version_is_written_to_packaged_help(self):
        with tempfile.TemporaryDirectory(prefix="tpx-package-test-") as temporary:
            root = Path(temporary)
            binary = root / "built-TpX"
            binary.write_bytes(b"Version 2.0.0-beta.1")
            package = root / "package"
            package.mkdir()
            build_candidate.stage_runtime(package, binary, "linux-x86_64", "2.0.0-beta.1")
            help_file = package / "help/tpx_tpxabout_tpx_drawing_tool.htm"
            self.assertIn(b'content="2.0.0-beta.1"', help_file.read_bytes())


class ReleaseCommandTests(unittest.TestCase):
    def test_stable_draft_keeps_reviewed_release_notes(self):
        with tempfile.TemporaryDirectory(prefix="tpx-release-command-") as temporary:
            root = Path(temporary)
            notes = root / "RELEASE_NOTES.md"
            notes.write_text("Reviewed stable notes.\n")
            assets = root / "assets"
            assets.mkdir()
            (assets / "package.zip").write_bytes(b"package")
            args = publish_release.create_command(
                "v1.6.1", "owner/repo", "b" * 40, "1.6.1",
                False, False, notes, assets)
            self.assertIn("--generate-notes", args)
            self.assertEqual(args[args.index("--notes") + 1], "Reviewed stable notes.")
            self.assertIn("--latest=false", args)
            self.assertNotIn("--prerelease", args)

    def test_prerelease_draft_keeps_reviewed_and_generated_notes(self):
        with tempfile.TemporaryDirectory(prefix="tpx-release-command-") as temporary:
            root = Path(temporary)
            notes = root / "RELEASE_NOTES.md"
            notes.write_text("Reviewed release notes.\n")
            assets = root / "assets"
            assets.mkdir()
            (assets / "package.zip").write_bytes(b"package")
            calls = []
            command = publish_release.create_command(
                "v2.0.0-beta.1", "owner/repo", "a" * 40, "2.0.0-beta.1",
                "true", "true", notes, assets)
            publish_release.run(command, runner=lambda args, **kwargs: calls.append((args, kwargs)))
            args, kwargs = calls[0]
            self.assertEqual(kwargs, {"check": True})
            self.assertIn("--generate-notes", args)
            self.assertEqual(args[args.index("--notes") + 1],
                             "Pre-release of TpX 2.0.0-beta.1. This build is for evaluation "
                             "and is not a stable release.\n\nReviewed release notes.")
            self.assertIn("--latest=false", args)
            self.assertIn("--prerelease", args)

    def test_final_publication_explicitly_sets_latest_and_prerelease(self):
        cases = ((False, True, "--latest=true", False),
                 (False, False, "--latest=false", False),
                 (True, True, "--latest=false", True))
        for prerelease, latest, expected_latest, expected_prerelease in cases:
            with self.subTest(prerelease=prerelease, latest=latest):
                calls = []
                command = publish_release.publish_command(
                    "v2.0.0", "owner/repo", prerelease, latest)
                publish_release.run(command, runner=lambda args, **kwargs: calls.append((args, kwargs)))
                args, kwargs = calls[0]
                self.assertEqual(kwargs, {"check": True})
                self.assertIn(expected_latest, args)
                self.assertEqual("--prerelease" in args, expected_prerelease)


class ReleaseArtifactTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="tpx-release-test-")
        self.directory = Path(self.temporary.name)
        self.commit = subprocess.check_output(["git", "rev-parse", "HEAD"],
                                              cwd=ROOT, text=True).strip()
        self.old_env = {key: os.environ.get(key) for key in
                        ("GITHUB_RUN_ID", "GITHUB_RUN_ATTEMPT")}
        os.environ.update(GITHUB_RUN_ID="123", GITHUB_RUN_ATTEMPT="1")

    def tearDown(self):
        for key, value in self.old_env.items():
            if value is None:
                os.environ.pop(key, None)
            else:
                os.environ[key] = value
        self.temporary.cleanup()

    def create_artifacts(self, *, wrong_sha=None, missing_check=None,
                         binary_version="2.0.0-beta.1", help_version="2.0.0-beta.1",
                         bundle_short_version="2.0.0", document_extensions=("tpx",)):
        version = "2.0.0-beta.1"
        source = subprocess.check_output(["git", "archive", "--format=zip", self.commit], cwd=ROOT)
        for platform, suffix in verify_release.PLATFORMS:
            name = f"tpx-{version}-{platform}"
            if platform.startswith("macos-"):
                executable = "TpX.app/Contents/MacOS/TpX"
                location = "TpX.app/Contents/Resources/"
            else:
                executable = "TpX.exe" if platform.startswith("windows-") else "TpX"
                location = ""
            checks = sorted(verify_release.BASE_CHECKS)
            if platform.startswith("linux-"):
                checks += ["Linux desktop MIME and user registration",
                           "TeX compilation", "packaged TeX compilation"]
            if platform.startswith("macos-"):
                checks += ["native Cocoa font cancellation", "native Cocoa text shortcuts",
                           "ad hoc signature verification", "signature tamper detection",
                           "Cocoa document type and app icon"]
            if missing_check and platform == missing_check[0]:
                checks.remove(missing_check[1])
            manifest = {"status": "release", "release_status": "prerelease",
                        "proposed_version": version, "commit": wrong_sha or self.commit,
                        "platform": platform, "executable": executable,
                        "run_id": "123", "run_attempt": "1", "checks": checks,
                        "macos_signing": "ad hoc" if platform.startswith("macos-") else None,
                        "macos_notarized": False if platform.startswith("macos-") else None,
                        **verify_release.toolchain(platform)}
            files = {"BUILD.json": (json.dumps(manifest) + "\n").encode(),
                     executable: f"Version {binary_version}".encode(),
                     location + "help/tpx_tpxabout_tpx_drawing_tool.htm":
                         f'<meta name="tpx-version" content="{help_version}">'.encode(),
                     location + "preview.tex.inc": b"preview",
                     location + "metapost.tex.inc": b"metapost",
                     "README.md": b"readme", "INSTALL.md": b"install",
                     "LICENSE": b"license", "THIRD_PARTY.md": b"third party",
                     "licenses/Lazarus-COPYING": b"license", "source.zip": source}
            if platform.startswith("macos-"):
                files["TpX.app/Contents/_CodeSignature/CodeResources"] = b"signature"
                files["TpX.app/Contents/Resources/TpX.icns"] = b"icon"
                files["TpX.app/Contents/Info.plist"] = plistlib.dumps({
                    "CFBundleIdentifier": "org.tpx.TpX",
                    "CFBundleShortVersionString": bundle_short_version,
                    "CFBundleVersion": "2.0.0",
                    "CFBundleIconFile": "TpX.icns",
                    "UTExportedTypeDeclarations": [{
                        "UTTypeIdentifier": "org.tpx.TpX.drawing",
                        "UTTypeTagSpecification": {
                            "public.filename-extension": list(document_extensions),
                            "public.mime-type": "application/x-tpx",
                        },
                    }],
                    "CFBundleDocumentTypes": [{
                        "LSItemContentTypes": ["org.tpx.TpX.drawing"],
                        "CFBundleTypeRole": "Editor",
                        "CFBundleTypeIconFile": "TpX.icns",
                        "LSHandlerRank": "Owner",
                    }],
                })
            if platform.startswith("linux-"):
                files.update({"tpx.svg": b"svg",
                              "org.tpx.TpX.desktop.in": b"desktop",
                              "org.tpx.TpX.xml": b"mime",
                              "install-linux-desktop-entry.sh": b"installer"})
            archive = self.directory / (name + suffix)
            with tempfile.TemporaryDirectory() as package_root:
                package = Path(package_root) / name
                package.mkdir()
                for relative, data in files.items():
                    target = package / relative
                    target.parent.mkdir(parents=True, exist_ok=True)
                    target.write_bytes(data)
                if suffix == ".zip":
                    with zipfile.ZipFile(archive, "w", zipfile.ZIP_DEFLATED) as output:
                        for path in package.rglob("*"):
                            if path.is_file():
                                output.write(path, path.relative_to(package.parent).as_posix())
                else:
                    with tarfile.open(archive, "w:gz") as output:
                        for path in package.rglob("*"):
                            if path.is_file():
                                output.add(path, path.relative_to(package.parent).as_posix())
            digest = hashlib.sha256(archive.read_bytes()).hexdigest()
            (self.directory / (archive.name + ".sha256")).write_bytes(
                f"{digest}  {archive.name}\n".encode("ascii"))

    def verify_fails(self, **kwargs):
        self.create_artifacts(**kwargs)
        with self.assertRaises(ValueError):
            verify_release.verify(self.directory, "2.0.0-beta.1", self.commit)

    def test_verifies_all_packages_and_emits_source_checksums(self):
        self.create_artifacts()
        verify_release.verify(self.directory, "2.0.0-beta.1", self.commit)
        source = self.directory / "tpx-2.0.0-beta.1-source.zip"
        source_hash = hashlib.sha256(source.read_bytes()).hexdigest()
        self.assertEqual((self.directory / (source.name + ".sha256")).read_bytes(),
                         f"{source_hash}  {source.name}\n".encode("ascii"))
        sums = {name: digest for digest, name in
                (line.split("  ", 1) for line in
                 (self.directory / "SHA256SUMS").read_text().splitlines())}
        for archive in self.directory.iterdir():
            if archive.name.endswith((".zip", ".tar.gz")):
                self.assertEqual(sums[archive.name], hashlib.sha256(archive.read_bytes()).hexdigest())

    def test_rejects_wrong_source_sha(self):
        self.verify_fails(wrong_sha="f" * 40)

    def test_rejects_missing_mandatory_tests(self):
        self.verify_fails(missing_check=("linux-x86_64", "TeX compilation"))

    def test_rejects_tampered_archives(self):
        self.create_artifacts()
        archive = self.directory / "tpx-2.0.0-beta.1-windows-x86_64.zip"
        archive.write_bytes(archive.read_bytes() + b"tamper")
        with self.assertRaisesRegex(ValueError, "checksum mismatch"):
            verify_release.verify(self.directory, "2.0.0-beta.1", self.commit)

    def test_rejects_wrong_binary_version(self):
        self.verify_fails(binary_version="2.0.0")

    def test_rejects_wrong_help_version(self):
        self.verify_fails(help_version="2.0.0")

    def test_rejects_prerelease_suffix_in_macos_short_version(self):
        self.verify_fails(bundle_short_version="2.0.0-beta.1")

    def test_rejects_generic_macos_document_associations(self):
        self.verify_fails(document_extensions=("tpx", "tex"))

    def test_rejects_incorrect_archive_version_names(self):
        self.create_artifacts()
        for path in list(self.directory.iterdir()):
            if "windows-x86_64" in path.name:
                path.rename(path.with_name(path.name.replace("2.0.0-beta.1", "2.0.0")))
        with self.assertRaisesRegex(ValueError, "exactly the three platform archives"):
            verify_release.verify(self.directory, "2.0.0-beta.1", self.commit)


if __name__ == "__main__":
    unittest.main(verbosity=2)
