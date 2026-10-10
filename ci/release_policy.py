#!/usr/bin/env python3
"""Validate the release branch/version contract and latest-release choice."""
import json
import os
from pathlib import Path
import re
import subprocess
import sys


TAG_VERSION = re.compile(r"^v([0-9]+)\.([0-9]+)\.([0-9]+)(?:-(beta|rc)\.([1-9][0-9]*))?$")
ABOUT_VERSION = re.compile(r"^\s*About_Version\s*=\s*'Version ([^']+)'", re.MULTILINE)


def parse_version(version):
    match = TAG_VERSION.fullmatch("v" + version)
    if not match:
        raise ValueError(f"invalid release version: {version}")
    major, minor, patch, stage, sequence = match.groups()
    return (int(major), int(minor), int(patch), stage, int(sequence or 0))


def is_prerelease(version):
    return parse_version(version)[3] is not None


def validate_release(ref, version, about_version, tags, *, published_tags=None,
                     source_sha=None, is_ancestor=None, is_linear=None):
    """Return release metadata, rejecting unsupported refs and tag history."""
    parsed = parse_version(version)
    tag = "v" + version
    tags = set(tags)
    published_tags = set(published_tags or ())
    if ref not in {"refs/heads/release/1.6", "refs/heads/release/2.0"}:
        raise ValueError(f"release is not allowed from {ref}")
    if about_version != version:
        raise ValueError(f"About version {about_version!r} != release {version!r}")
    if tag in tags:
        raise ValueError(f"release tag already exists: {tag}")

    if ref == "refs/heads/release/1.6":
        if parsed[:2] != (1, 6) or parsed[3] is not None or parsed[2] == 0:
            raise ValueError("release/1.6 accepts stable 1.6.x patch releases only")
        if "v1.6.0" not in tags:
            raise ValueError("release/1.6 requires the v1.6.0 bootstrap tag")
        if source_sha and is_ancestor and not is_ancestor("v1.6.0", source_sha):
            raise ValueError("release/1.6 source must descend from v1.6.0")
        if source_sha and is_linear and not is_linear("v1.6.0", source_sha):
            raise ValueError("release/1.6 must contain selective single-parent backports")
    else:
        if parsed[:3] != (2, 0, 0):
            raise ValueError("release/2.0 accepts 2.0.0 prereleases and stable only")
        beta_tags = sorted(
            parse_version(name[1:])[4] for name in tags
            if name.startswith("v2.0.0-beta.")
        )
        rc_tags = sorted(
            parse_version(name[1:])[4] for name in tags
            if name.startswith("v2.0.0-rc.")
        )
        stage, sequence = parsed[3], parsed[4]
        if "v2.0.0" in tags:
            raise ValueError("2.0 prereleases cannot follow the stable release")
        if not beta_tags and not rc_tags and version != "2.0.0-beta.1":
            raise ValueError("the first 2.0 release must bootstrap with beta.1")
        if stage == "beta":
            if rc_tags or (beta_tags and sequence != max(beta_tags) + 1):
                raise ValueError("2.0 beta tags must advance before the first RC")
        elif stage == "rc":
            if (not beta_tags or f"v2.0.0-beta.{max(beta_tags)}" not in published_tags or
                    (sequence != (max(rc_tags) + 1 if rc_tags else 1))):
                raise ValueError("2.0 RC tags require the latest beta to be published and advance in order")
        elif not rc_tags or f"v2.0.0-rc.{max(rc_tags)}" not in published_tags:
            raise ValueError("2.0.0 stable requires a published release candidate")

    return {"tag": tag, "prerelease": parsed[3] is not None}


def should_mark_latest(version, releases):
    """Keep prereleases and older maintenance releases out of Latest."""
    parsed = parse_version(version)
    if parsed[3] is not None:
        return False
    if releases and isinstance(releases[0], list):
        releases = (release for page in releases for release in page)
    stable = []
    for release in releases:
        if release.get("draft") or release.get("prerelease"):
            continue
        name = release.get("tag_name", "")
        if name.startswith("v"):
            try:
                candidate = parse_version(name[1:])
            except ValueError:
                continue
            if candidate[3] is None:
                stable.append(candidate[:3])
    return not stable or parsed[:3] >= max(stable)


def ancestor(base, commit):
    return subprocess.run(["git", "merge-base", "--is-ancestor", base, commit],
                          check=False).returncode == 0


def linear_history(base, commit, *, cwd=None):
    history = subprocess.check_output(
        ["git", "rev-list", "--parents", f"{base}..{commit}"], text=True, cwd=cwd)
    return all(len(line.split()) == 2 for line in history.splitlines())


def preflight(version):
    ref = os.environ["GITHUB_REF"]
    source_sha = os.environ["GITHUB_SHA"]
    checked_out = subprocess.check_output(["git", "rev-parse", "HEAD"], text=True).strip()
    if checked_out != source_sha:
        raise ValueError(f"checkout {checked_out} != approved source {source_sha}")
    source = Path("src/tpx.inc").read_text(encoding="utf-8")
    match = ABOUT_VERSION.search(source)
    if not match:
        raise ValueError("src/tpx.inc has no About_Version")
    tags = subprocess.check_output(["git", "tag", "--list"], text=True).splitlines()
    pages = json.loads(subprocess.check_output(
        ["gh", "api", "--paginate", "--slurp",
         f"repos/{os.environ['GITHUB_REPOSITORY']}/releases"], text=True))
    releases = (release for page in pages for release in page)
    published_tags = {release["tag_name"] for release in releases
                      if not release["draft"] and release["prerelease"]}
    metadata = validate_release(ref, version, match.group(1), tags,
                                published_tags=published_tags,
                                source_sha=source_sha, is_ancestor=ancestor,
                                is_linear=linear_history)
    print(f"version={version}")
    print(f"tag={metadata['tag']}")
    print(f"prerelease={str(metadata['prerelease']).lower()}")


if __name__ == "__main__":
    if sys.argv[1] == "preflight":
        preflight(sys.argv[2])
    elif sys.argv[1] == "latest":
        releases = json.loads(Path(sys.argv[3]).read_text(encoding="utf-8"))
        print(f"latest={str(should_mark_latest(sys.argv[2], releases)).lower()}")
    else:
        raise SystemExit("usage: release_policy.py preflight VERSION | latest VERSION RELEASES.json")
