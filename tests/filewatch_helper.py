#!/usr/bin/env python3
"""Barrier-controlled writer process for the native file-watch tests."""

import os
from pathlib import Path
import sys


def apply(path: Path, command: str) -> None:
    if command == "overwrite":
        path.write_bytes(b"overwritten content\n")
    elif command == "truncate":
        with path.open("r+b") as stream:
            stream.truncate(0)
            stream.write(b"rewritten after truncate\n")
    elif command.startswith("atomic"):
        temporary = path.with_name(path.name + ".tmp")
        temporary.write_bytes((command + " content\n").encode())
        os.replace(temporary, path)
    elif command == "delete":
        path.unlink()
    elif command == "create":
        path.write_bytes(b"created again\n")
    elif command == "away_back":
        away = path.with_name(path.name + ".away")
        os.replace(path, away)
        os.replace(away, path)
    elif command == "sibling":
        path.with_name(path.name + ".unrelated").write_bytes(b"sibling\n")
    elif command == "same_size_mtime":
        old = path.stat()
        original = path.read_bytes()
        replacement = bytes((byte ^ 1) for byte in original)
        path.write_bytes(replacement)
        os.utime(path, ns=(old.st_atime_ns, old.st_mtime_ns))
        if replacement == original or path.stat().st_mtime_ns != old.st_mtime_ns:
            raise RuntimeError("same-size/restored-mtime precondition failed")
    else:
        raise ValueError(f"unknown helper command: {command}")


def main() -> int:
    path = Path(sys.argv[1])
    print("READY", flush=True)
    for line in sys.stdin:
        command = line.strip()
        if command == "STOP":
            print("STOPPED", flush=True)
            return 0
        try:
            apply(path, command)
            print(f"ACK {command}", flush=True)
        except Exception as error:  # report to the test controller
            print(f"ERROR {command} {error}", flush=True)
            return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
