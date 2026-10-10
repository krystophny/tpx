#!/usr/bin/env python3
"""Build the checked release publication commands and execute the GitHub CLI."""
from pathlib import Path
import subprocess
import sys


def boolean(value, name):
    if value in (True, "true"):
        return True
    if value in (False, "false"):
        return False
    raise ValueError(f"{name} must be true or false")


def release_notes(version, prerelease, reviewed_notes):
    reviewed_notes = reviewed_notes.strip()
    if prerelease:
        notice = (f"Pre-release of TpX {version}. This build is for evaluation "
                  "and is not a stable release.")
        return f"{notice}\n\n{reviewed_notes}"
    return reviewed_notes


def create_command(tag, repository, source_sha, version, prerelease, latest,
                   notes_path, asset_directory):
    if tag != f"v{version}":
        raise ValueError("release tag must match the requested version")
    prerelease = boolean(prerelease, "prerelease")
    latest = False if prerelease else boolean(latest, "latest")
    assets = sorted(path for path in Path(asset_directory).iterdir() if path.is_file())
    if not assets:
        raise ValueError("release assets are missing")
    notes = release_notes(version, prerelease,
                          Path(notes_path).read_text(encoding="utf-8"))
    args = ["gh", "release", "create", tag, *map(str, assets),
            "--repo", repository, "--verify-tag", "--target", source_sha,
            "--title", f"TpX {version}", "--generate-notes", "--notes", notes,
            "--draft", f"--latest={str(latest).lower()}"]
    if prerelease:
        args.append("--prerelease")
    return args


def publish_command(tag, repository, prerelease, latest):
    prerelease = boolean(prerelease, "prerelease")
    latest = False if prerelease else boolean(latest, "latest")
    args = ["gh", "release", "edit", tag, "--repo", repository,
            "--draft=false", f"--latest={str(latest).lower()}"]
    if prerelease:
        args.append("--prerelease")
    return args


def run(command, runner=subprocess.run):
    runner(command, check=True)


def main(argv):
    if len(argv) == 10 and argv[1] == "create":
        command = create_command(*argv[2:])
    elif len(argv) == 6 and argv[1] == "publish":
        command = publish_command(*argv[2:])
    else:
        raise SystemExit(
            "usage: publish_release.py create TAG REPOSITORY SHA VERSION "
            "PRERELEASE LATEST NOTES_FILE ASSET_DIR | publish TAG REPOSITORY "
            "PRERELEASE LATEST")
    run(command)


if __name__ == "__main__":
    main(sys.argv)
