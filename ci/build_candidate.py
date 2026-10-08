#!/usr/bin/env python3
"""Build checked portable candidates; deliberately has no release operation."""
import hashlib
import json
import os
from pathlib import Path
import plistlib
import re
import shutil
import subprocess
import sys
import tempfile

ROOT = Path(__file__).resolve().parents[1]


def command(*args, **kwargs):
    return subprocess.run(list(map(str, args)), check=True, cwd=ROOT, **kwargs)


def output(*args):
    return command(*args, capture_output=True, text=True).stdout.strip()


def check_exports(binary):
    environment = dict(os.environ, TPX_BINARY=str(binary.resolve()))
    command(sys.executable, "tests/test_exports.py", env=environment)
    if os.environ["CANDIDATE_PLATFORM"].startswith("linux-"):
        # Require the tool, so missing TeX cannot turn this gate into a skip.
        if not shutil.which("pdflatex"):
            raise RuntimeError("Linux candidates require pdflatex for the TeX gate")
        if not shutil.which("sam2p"):
            raise RuntimeError("Linux candidates require sam2p for the bitmap gate")
        command(sys.executable, "tests/test_tex.py", env=environment)
    else:
        print("TeX compilation is covered by the Linux candidate job; "
              "this job checks native exports and GUI scenarios.", flush=True)


def stage_runtime(package, binary, platform, version):
    """Put TpX and its editable preambles where the executable finds them."""
    executable_dir = package
    if platform.startswith("macos-"):
        contents = package / "TpX.app" / "Contents"
        executable_dir = contents / "MacOS"
        executable_dir.mkdir(parents=True)
        bundle_version = version.split("-", 1)[0]
        with (contents / "Info.plist").open("wb") as stream:
            plistlib.dump({
                "CFBundleIdentifier": "org.tpx.TpX",
                "CFBundleName": "TpX",
                "CFBundleDisplayName": "TpX",
                "CFBundleExecutable": binary.name,
                "CFBundlePackageType": "APPL",
                "CFBundleInfoDictionaryVersion": "6.0",
                "CFBundleShortVersionString": bundle_version,
                "CFBundleVersion": bundle_version,
                "NSHighResolutionCapable": True,
            }, stream)
    packaged_binary = executable_dir / binary.name
    shutil.copy2(binary, packaged_binary)
    for filename in ("preview.tex.inc", "metapost.tex.inc"):
        shutil.copy2(ROOT / "ci" / filename, executable_dir / filename)
    return packaged_binary


def build_and_package():
    version = os.environ["CANDIDATE_VERSION"]
    if not re.fullmatch(r"[0-9]+\.[0-9]+\.[0-9]+(?:-[A-Za-z0-9.]+)?", version):
        raise ValueError("version must be a numeric major.minor.patch with optional suffix")
    platform = os.environ["CANDIDATE_PLATFORM"]
    widgetset = os.environ["WIDGETSET"]
    if (platform, widgetset) not in {
        ("windows-x86_64", "win32"), ("linux-x86_64", "gtk2"),
        ("macos-aarch64", "cocoa"),
    }:
        raise ValueError("unsupported platform/widgetset pair")
    lazarus = Path(os.environ["LAZARUS_DIR"])
    fpc = os.environ.get("FPC", "fpc")
    cpu, operating_system = output(fpc, "-iTP"), output(fpc, "-iTO")
    expected_target = {"windows-x86_64": ("x86_64", "win64"),
                       "linux-x86_64": ("x86_64", "linux"),
                       "macos-aarch64": ("aarch64", "darwin")}[platform]
    if (cpu, operating_system) != expected_target:
        raise RuntimeError(f"compiler target {(cpu, operating_system)} != {expected_target}")
    build = [os.environ["LAZBUILD"], "--build-all", f"--ws={widgetset}",
             f"--lazarusdir={lazarus}", f"--pcp={ROOT / '.lazarus'}"]
    if os.environ.get("TPX_COMPILER"):
        build.append("--compiler=" + os.environ["TPX_COMPILER"])
    command(*build, "TpX.lpi")
    command(*build, "tests/RuntimeTests.lpi")
    if platform.startswith("macos-"):
        command("sh", lazarus / "test/lcltests/testcocoafontdialog.sh", lazarus)
    suffix = ".exe" if platform.startswith("windows-") else ""
    binary_dir = ROOT / "obj" / f"{cpu}-{operating_system}"
    binary = binary_dir / ("TpX" + suffix)
    runtime = binary_dir / ("RuntimeTests" + suffix)
    check_exports(binary)
    command(sys.executable, "tests/test_runtime.py",
            env=dict(os.environ, RUNTIME_BINARY=str(runtime)))

    commit = output("git", "rev-parse", "HEAD")
    name = f"tpx-{version}-candidate-{platform}-{commit[:12]}"
    dist = ROOT / "dist"
    dist.mkdir(exist_ok=True)
    with tempfile.TemporaryDirectory(prefix="tpx candidate ") as temporary:
        package = Path(temporary) / name
        package.mkdir()
        packaged_binary = stage_runtime(package, binary, platform, version)
        for filename in ("README.md", "LICENSE"):
            shutil.copy2(ROOT / filename, package / filename)
        for filename in ("CANDIDATE.md", "THIRD_PARTY.md"):
            shutil.copy2(ROOT / "ci" / filename, package / filename)
        licenses = package / "licenses"
        licenses.mkdir()
        for directory in (lazarus, lazarus / "lcl"):
            for path in directory.glob("COPYING*"):
                if path.is_file():
                    shutil.copy2(path, licenses / (directory.name + "-" + path.name))
        if not any(licenses.iterdir()):
            raise RuntimeError("Lazarus distribution license texts are missing")
        # Embedded form resources are linked into TpX. Include the exact project
        # sources, including vendored component license notices, beside the binary.
        command("git", "archive", "--format=zip", "HEAD", "-o", package / "source.zip")
        manifest = {
            "status": "candidate; manual playtest required before release",
            "proposed_version": version, "commit": commit, "platform": platform,
            "executable": packaged_binary.relative_to(package).as_posix(),
            "widgetset": widgetset, "fpc_version": output(fpc, "-iV"),
            "lazarus_source": os.environ.get("LAZARUS_SOURCE", "local"),
            "lazarus_patch_sha256": hashlib.sha256(
                (ROOT / "ci/patches/lazarus-4.8-opendocument.patch").read_bytes()
            ).hexdigest() if operating_system != "win64" else None,
            "lazarus_patches_sha256": {
                name: hashlib.sha256((ROOT / "ci/patches" / name).read_bytes()).hexdigest()
                for name in ("lazarus-4.8-opendocument.patch",
                             "lazarus-4.8-cocoa-font-cancel.patch")
            } if operating_system != "win64" else {},
            "fpc_source": os.environ.get("FPC_SOURCE", "3.2.2 distribution"),
            "sam2p_source": os.environ.get("SAM2P_SOURCE"),
            "run_id": os.environ.get("GITHUB_RUN_ID", "local"),
            "run_attempt": os.environ.get("GITHUB_RUN_ATTEMPT", "local"),
            "checks": ["native exports", "native GUI scenarios", "packaged exports"],
        }
        if platform.startswith("linux-"):
            manifest["checks"].extend(["TeX compilation", "packaged TeX compilation"])
        if platform.startswith("macos-"):
            manifest["checks"].append("native Cocoa font cancellation")
        (package / "BUILD.json").write_text(json.dumps(manifest, indent=2) + "\n")
        fmt = "zip" if platform.startswith("windows-") else "gztar"
        archive = Path(shutil.make_archive(str(dist / name), fmt,
                                          root_dir=temporary, base_dir=name))
        extracted = Path(temporary) / "extracted candidate"
        shutil.unpack_archive(str(archive), extracted)
        # Test the actual extracted archive from a path containing spaces.
        check_exports(extracted / name / packaged_binary.relative_to(package))
    checksum = hashlib.sha256(archive.read_bytes()).hexdigest()
    (dist / (archive.name + ".sha256")).write_text(f"{checksum}  {archive.name}\n")
    print(f"Checked candidate: {archive.name}\nSHA256: {checksum}", flush=True)


if __name__ == "__main__":
    build_and_package()
