#!/usr/bin/env python3
"""Perform one filesystem mutation from a process outside the watcher."""

import os
from pathlib import Path
import sys
import time
import uuid


def write(path: Path, data: bytes) -> None:
    with path.open("wb") as stream:
        stream.write(data)


def replace(path: Path, data: bytes) -> None:
    temporary = path.with_name(path.name + ".replacement-" + uuid.uuid4().hex)
    write(temporary, data)
    os.replace(temporary, path)


def main() -> None:
    action, raw_path = sys.argv[1:]
    path = Path(raw_path)
    if action == "overwrite":
        write(path, b"external in-place revision " + uuid.uuid4().bytes)
    elif action == "same-metadata":
        before = path.stat()
        original = path.read_bytes()
        changed = bytes([original[0] ^ 1]) + original[1:] if original else b"x"
        if len(original) == 0:
            write(path, changed)
        else:
            write(path, changed)
        os.utime(path, ns=(before.st_atime_ns, before.st_mtime_ns))
    elif action == "replace":
        replace(path, b"atomic replacement " + uuid.uuid4().bytes)
    elif action == "replace-storm":
        for _ in range(40):
            replace(path, b"re-arm race " + uuid.uuid4().bytes)
            time.sleep(0.001)
    elif action == "delete":
        path.unlink()
    elif action == "create":
        write(path, b"recreated source " + uuid.uuid4().bytes)
    elif action == "rename-cycle":
        away = path.with_name(path.name + ".renamed-away-" + uuid.uuid4().hex)
        os.rename(path, away)
        os.rename(away, path)
    elif action == "sibling":
        sibling = path.with_name(path.name + ".unrelated-" + uuid.uuid4().hex)
        write(sibling, b"unrelated")
    elif action == "replace-target":
        target = path.resolve(strict=True)
        replace(target, b"replaced symlink target " + uuid.uuid4().bytes)
    elif action == "overwrite-case-peer":
        peer_name = path.name[0].swapcase() + path.name[1:]
        write(path.with_name(peer_name), b"case peer revision " + uuid.uuid4().bytes)
    else:
        raise SystemExit("unknown action: " + action)


if __name__ == "__main__":
    main()
