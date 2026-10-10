#!/usr/bin/env python3
"""Verify matrix archives against the checked-out release source before upload."""
import hashlib
import io
import json
import os
from pathlib import Path
from pathlib import PurePosixPath
import subprocess
import tarfile
import zipfile

ROOT = Path(__file__).resolve().parents[1]
PLATFORMS = (("windows-x86_64", ".zip"), ("linux-x86_64", ".tar.gz"),
             ("macos-aarch64", ".zip"))
BASE_CHECKS = {"native exports", "native GUI scenarios", "packaged exports"}
LAZARUS_COMMIT = "62c14a4d18c81f222127d42ce9b89b922c63fcbf"
FPC_COMMIT = "0eeb544f39332bfe6e468c8cae18bbf6270c7e1e"
SAM2P_COMMIT = "9b0f8b77b5fb0af42e8faf8fb63a0760e214757b"
WINDOWS_INSTALLER_SHA256 = "ED25EE171D55E23CF14E0633159FDD2325EFBA56E186F8AB817AD3BF97D267D7"


def require(condition, message):
    if not condition:
        raise ValueError(message)


def members(path):
    if path.name.endswith(".zip"):
        with zipfile.ZipFile(path) as archive:
            return {name: archive.read(name) for name in archive.namelist()
                    if not name.endswith("/")}
    with tarfile.open(path) as archive:
        return {entry.name: archive.extractfile(entry).read()
                for entry in archive.getmembers() if entry.isfile()}


def toolchain(platform):
    if platform == "windows-x86_64":
        return {"fpc_version": "3.2.2", "fpc_source": "3.2.2 distribution",
                "lazarus_source": f"4.8 official installer SHA256:{WINDOWS_INSTALLER_SHA256}",
                "lazarus_patch_sha256": None, "lazarus_patches_sha256": {},
                "sam2p_source": None}
    patches = {
        name: hashlib.sha256((ROOT / "ci/patches" / name).read_bytes()).hexdigest()
        for name in ("lazarus-4.8-opendocument.patch",
                     "lazarus-4.8-cocoa-font-cancel.patch",
                     "lazarus-4.8-cocoa-text-shortcuts.patch")
    }
    return {
        "fpc_version": "3.2.3" if platform.startswith("macos-") else "3.2.2",
        "fpc_source": FPC_COMMIT if platform.startswith("macos-") else "3.2.2 distribution",
        "lazarus_source": LAZARUS_COMMIT,
        "lazarus_patch_sha256": patches["lazarus-4.8-opendocument.patch"],
        "lazarus_patches_sha256": patches,
        "sam2p_source": SAM2P_COMMIT if platform.startswith("linux-") else None,
    }


def verify(directory, version, commit):
    prerelease = "-" in version
    expected_assets = {f"tpx-{version}-{platform}{suffix}"
                       for platform, suffix in PLATFORMS}
    expected_sidecars = {name + ".sha256" for name in expected_assets}
    require({path.name for path in directory.iterdir()} == expected_assets | expected_sidecars,
            "release input must contain exactly the three platform archives and checksums")
    canonical = subprocess.check_output(["git", "archive", "--format=zip", commit], cwd=ROOT)
    with zipfile.ZipFile(io.BytesIO(canonical)) as archive:
        sources = {name: archive.read(name) for name in archive.namelist()
                   if not name.endswith("/")}
    for platform, suffix in PLATFORMS:
        name = f"tpx-{version}-{platform}"
        path = directory / (name + suffix)
        digest = hashlib.sha256(path.read_bytes()).hexdigest()
        sidecar = f"{digest}  {path.name}\n".encode("ascii")
        require((directory / (path.name + ".sha256")).read_bytes() == sidecar,
                f"archive checksum mismatch: {path.name}")
        files = members(path)
        prefix = name + "/"
        manifest = json.loads(files[prefix + "BUILD.json"])
        require(manifest["status"] == "release", f"{platform}: wrong build status")
        require(manifest["release_status"] == ("prerelease" if prerelease else "stable"),
                f"{platform}: wrong stable/prerelease metadata")
        require(manifest["commit"] == commit, f"{platform}: wrong source SHA")
        require(manifest["proposed_version"] == version, f"{platform}: wrong package version")
        require(manifest["platform"] == platform, f"{platform}: wrong platform metadata")
        require(manifest["run_id"] == os.environ["GITHUB_RUN_ID"], f"{platform}: wrong run ID")
        require(manifest["run_attempt"] == os.environ["GITHUB_RUN_ATTEMPT"],
                f"{platform}: wrong run attempt")
        required_checks = set(BASE_CHECKS)
        if platform.startswith("macos-"):
            required_checks |= {"native Cocoa font cancellation", "native Cocoa text shortcuts",
                                "ad hoc signature verification", "signature tamper detection"}
        if platform.startswith("linux-"):
            required_checks |= {"TeX compilation", "packaged TeX compilation"}
        require(required_checks <= set(manifest["checks"]),
                f"{platform}: missing required tests {required_checks - set(manifest['checks'])}")
        expected_toolchain = toolchain(platform)
        require(all(manifest.get(key) == value for key, value in expected_toolchain.items()),
                f"{platform}: toolchain pin or checksum mismatch")
        executable_path = PurePosixPath(manifest["executable"])
        require(not executable_path.is_absolute() and ".." not in executable_path.parts,
                f"{platform}: executable path escapes package")
        executable = prefix + executable_path.as_posix()
        require(files.get(executable),
                f"{platform}: missing executable")
        require(f"Version {version}".encode("ascii") in files[executable],
                f"{platform}: executable About version mismatch")
        location = (prefix + "TpX.app/Contents/Resources/" if platform.startswith("macos-")
                    else executable.rsplit("/", 1)[0] + "/")
        help_path = location + "help/tpx_tpxabout_tpx_drawing_tool.htm"
        require(f'name="tpx-version" content="{version}"'.encode("ascii") in files.get(help_path, b""),
                f"{platform}: offline help version mismatch")
        for required in ("preview.tex.inc", "metapost.tex.inc",
                         "help/tpx_tpxabout_tpx_drawing_tool.htm"):
            require(files.get(location + required), f"{platform}: missing {required}")
        for required in ("INSTALL.md", "README.md", "LICENSE", "THIRD_PARTY.md"):
            require(files.get(prefix + required), f"{platform}: missing {required}")
        require(any(path.startswith(prefix + "licenses/") for path in files),
                f"{platform}: missing dependency licenses")
        with zipfile.ZipFile(io.BytesIO(files[prefix + "source.zip"])) as archive:
            archived = {entry: archive.read(entry) for entry in archive.namelist()
                        if not entry.endswith("/")}
        require(archived.keys() == sources.keys(), f"{platform}: source file list mismatch")
        for source, contents in sources.items():
            actual = archived[source]
            if b"\0" not in contents:
                actual, contents = actual.replace(b"\r\n", b"\n"), contents.replace(b"\r\n", b"\n")
            require(actual == contents, f"{platform}: source mismatch: {source}")
        if platform.startswith("macos-"):
            require(manifest["macos_signing"] == "ad hoc" and
                    manifest["macos_notarized"] is False, "unexpected macOS signing metadata")
            require(files.get(prefix + "TpX.app/Contents/_CodeSignature/CodeResources"),
                    "macOS app signature is missing")
        print(f"Verified {path.name}: {digest}")
    source = directory / f"tpx-{version}-source.zip"
    source.write_bytes(canonical)
    (directory / (source.name + ".sha256")).write_bytes(
        f"{hashlib.sha256(canonical).hexdigest()}  {source.name}\n".encode("ascii"))
    assets = sorted(path for path in directory.iterdir()
                    if path.name.endswith((".zip", ".tar.gz")))
    checksums = "".join(f"{hashlib.sha256(path.read_bytes()).hexdigest()}  {path.name}\n"
                        for path in assets)
    (directory / "SHA256SUMS").write_bytes(checksums.encode("ascii"))


if __name__ == "__main__":
    import sys
    verify(Path(sys.argv[1]), os.environ["RELEASE_VERSION"], os.environ["GITHUB_SHA"])
