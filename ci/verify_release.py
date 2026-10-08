#!/usr/bin/env python3
"""Verify matrix archives against the checked-out release source before upload."""
import hashlib
import io
import json
import os
from pathlib import Path
import subprocess
import tarfile
import zipfile

ROOT = Path(__file__).resolve().parents[1]


def members(path):
    if path.suffix == ".zip":
        with zipfile.ZipFile(path) as archive:
            return {name: archive.read(name) for name in archive.namelist()
                    if not name.endswith("/")}
    with tarfile.open(path) as archive:
        return {entry.name: archive.extractfile(entry).read()
                for entry in archive.getmembers() if entry.isfile()}


def verify(directory, version, commit):
    canonical = subprocess.check_output(["git", "archive", "--format=zip", commit], cwd=ROOT)
    with zipfile.ZipFile(io.BytesIO(canonical)) as archive:
        sources = {name: archive.read(name) for name in archive.namelist()
                   if not name.endswith("/")}
    for platform, suffix in (("windows-x86_64", ".zip"),
                             ("linux-x86_64", ".tar.gz"),
                             ("macos-aarch64", ".zip")):
        name = f"tpx-{version}-{platform}"
        path = directory / (name + suffix)
        digest = hashlib.sha256(path.read_bytes()).hexdigest()
        assert (directory / (path.name + ".sha256")).read_text().strip() == f"{digest}  {path.name}", "archive checksum mismatch"
        files = members(path)
        prefix = name + "/"
        manifest = json.loads(files[prefix + "BUILD.json"])
        assert manifest["status"] == "release"
        assert manifest["commit"] == commit
        assert manifest["proposed_version"] == version
        assert manifest["platform"] == platform
        assert manifest["run_id"] == os.environ["GITHUB_RUN_ID"]
        assert {"native exports", "native GUI scenarios", "packaged exports"} <= set(manifest["checks"])
        executable = prefix + manifest["executable"]
        assert files[executable], "empty executable"
        location = executable.rsplit("/", 1)[0] + "/"
        for required in ("preview.tex.inc", "metapost.tex.inc",
                         "help/tpx_tpxabout_tpx_drawing_tool.htm"):
            assert files[location + required], f"missing {required}"
        for required in ("INSTALL.md", "README.md", "LICENSE", "THIRD_PARTY.md"):
            assert files[prefix + required], f"missing {required}"
        assert any(name.startswith(prefix + "licenses/") for name in files)
        with zipfile.ZipFile(io.BytesIO(files[prefix + "source.zip"])) as archive:
            archived = {name: archive.read(name) for name in archive.namelist()
                        if not name.endswith("/")}
        # Git on Windows may check out CRLF. Binary content remains byte-exact.
        assert archived.keys() == sources.keys(), "source file list mismatch"
        for source, contents in sources.items():
            actual = archived[source]
            if b"\0" not in contents:
                actual, contents = actual.replace(b"\r\n", b"\n"), contents.replace(b"\r\n", b"\n")
            assert actual == contents, f"source mismatch: {source}"
        if platform.startswith("macos-"):
            assert {"ad hoc signature verification", "signature tamper detection"} <= set(manifest["checks"])
            assert manifest["macos_signing"] == "ad hoc"
            assert manifest["macos_notarized"] is False
            assert files[prefix + "TpX.app/Contents/_CodeSignature/CodeResources"]
        if platform.startswith("linux-"):
            assert {"TeX compilation", "packaged TeX compilation"} <= set(manifest["checks"])
        print(f"Verified {path.name}: {digest}")
    source = directory / f"tpx-{version}-source.zip"
    source.write_bytes(canonical)
    (directory / (source.name + ".sha256")).write_text(
        f"{hashlib.sha256(canonical).hexdigest()}  {source.name}\n")
    assets = sorted(path for path in directory.iterdir()
                    if path.name.endswith((".zip", ".tar.gz")))
    checksums = "".join(f"{hashlib.sha256(path.read_bytes()).hexdigest()}  {path.name}\n"
                        for path in assets)
    (directory / "SHA256SUMS").write_text(checksums)


if __name__ == "__main__":
    import sys
    verify(Path(sys.argv[1]), os.environ["RELEASE_VERSION"], os.environ["GITHUB_SHA"])
