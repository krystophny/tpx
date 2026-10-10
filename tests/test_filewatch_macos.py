#!/usr/bin/env python3
"""Headless native kqueue conformance tests using a separate writer process."""

from pathlib import Path
import os
import queue
import resource
import shutil
import subprocess
import sys
import tempfile
import threading
import unittest


ROOT = Path(__file__).resolve().parents[1]
HELPER = ROOT / "tests" / "FileWatchMacHelper.py"


class Watcher:
    def __init__(self, binary: Path, path: Path):
        self.process = subprocess.Popen(
            [str(binary), str(path)], stdin=subprocess.PIPE, stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT, text=True, bufsize=1,
            preexec_fn=limit_descriptors,
        )
        self.lines = queue.Queue()
        self.reader = threading.Thread(target=self._read_lines, daemon=True)
        self.reader.start()
        ready = self.read_line()
        if not ready.startswith("READY|"):
            raise AssertionError("watcher failed to start: " + ready)
        self.subscription_id = int(ready.split("|", 1)[1])

    def _read_lines(self):
        for line in self.process.stdout:
            self.lines.put(line.rstrip("\n"))
        self.lines.put(None)

    def read_line(self, timeout=5):
        line = self.lines.get(timeout=timeout)
        if line is None:
            raise AssertionError("watcher exited early: " + str(self.process.poll()))
        if line.startswith("FAIL|"):
            raise AssertionError(line)
        return line

    def command(self, command, timeout=5):
        self.process.stdin.write(command + "\n")
        self.process.stdin.flush()
        lines = []
        while True:
            line = self.read_line(timeout=timeout)
            if line == "END":
                return lines
            lines.append(line)

    def events(self, timeout=1600):
        return [line.split("|") for line in self.command("POLL " + str(timeout))
                if line.startswith("EVENT|")]

    def close(self):
        if self.process.poll() is None:
            self.process.stdin.write("STOP\n")
            self.process.stdin.flush()
            self.read_line()
        self.process.wait(timeout=5)
        self.process.stdin.close()
        self.process.stdout.close()
        if self.process.returncode != 0:
            raise AssertionError("watcher returned " + str(self.process.returncode))


def limit_descriptors():
    soft, hard = resource.getrlimit(resource.RLIMIT_NOFILE)
    resource.setrlimit(resource.RLIMIT_NOFILE, (min(64, hard), hard))


def mutate(action: str, path: Path):
    subprocess.run([sys.executable, str(HELPER), action, str(path)],
                   check=True, timeout=5, capture_output=True, text=True)


def event_kinds(events):
    return [event[1] for event in events]


class MacFileWatchTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.build = Path(tempfile.mkdtemp(prefix="tpx-filewatch-mac-build-"))
        fpc = os.environ.get("FPC") or shutil.which("fpc")
        if not fpc:
            raise unittest.SkipTest("Free Pascal compiler is required")
        api_dir = Path(os.environ.get("TPX_FILEWATCH_API_DIR", ROOT / "src"))
        command = [fpc, "-dTPX_FILEWATCH_TESTS", "-Fu" + str(api_dir),
                   "-Fu" + str(ROOT / "src"), "-FU" + str(cls.build),
                   "-FE" + str(cls.build), str(ROOT / "tests" / "FileWatchMacTests.lpr")]
        result = subprocess.run(command, cwd=ROOT, capture_output=True, text=True,
                                timeout=60)
        if result.returncode:
            raise AssertionError(result.stdout + result.stderr)
        cls.binary = cls.build / "FileWatchMacTests"

    @classmethod
    def tearDownClass(cls):
        shutil.rmtree(cls.build, ignore_errors=True)

    def setUp(self):
        self.directory = tempfile.TemporaryDirectory(prefix="tpx watch λ ")
        self.root = Path(self.directory.name)
        self.path = self.root / "drawing with spaces λ.tpx"
        self.path.write_bytes(b"initial source bytes")
        self.watcher = Watcher(self.binary, self.path)
        ready = self.watcher.events(timeout=0)
        self.assertIn("ready", event_kinds(ready))

    def tearDown(self):
        if hasattr(self, "watcher"):
            self.watcher.close()
        self.directory.cleanup()

    def test_file_and_parent_vnodes_survive_saves_and_lifecycle(self):
        mutate("overwrite", self.path)
        content = self.watcher.events()
        self.assertIn("content", event_kinds(content))
        self.assertTrue(all(event[5] == str(self.path) for event in content
                            if event[1] == "content"))

        mutate("replace", self.path)
        replaced = self.watcher.events()
        self.assertTrue(set(event_kinds(replaced)) & {"replaced", "reappeared"})

        mutate("replace", self.path)
        replaced_again = self.watcher.events()
        self.assertTrue(set(event_kinds(replaced_again)) & {"replaced", "reappeared"})

        mutate("overwrite", self.path)
        later_edit = self.watcher.events()
        self.assertIn("content", event_kinds(later_edit))
        self.assertTrue(all(int(event[3]) == 100 for event in later_edit))

        before = self.path.stat()
        mutate("same-metadata", self.path)
        self.assertEqual(self.path.stat().st_size, before.st_size)
        self.assertEqual(self.path.stat().st_mtime_ns, before.st_mtime_ns)
        self.assertIn("content", event_kinds(self.watcher.events()))

        mutate("delete", self.path)
        self.assertIn("disappeared", event_kinds(self.watcher.events()))
        mutate("create", self.path)
        self.assertIn("reappeared", event_kinds(self.watcher.events()))

        mutate("rename-cycle", self.path)
        self.watcher.events()
        mutate("overwrite", self.path)
        self.assertIn("content", event_kinds(self.watcher.events()))

        self.watcher.command("REVOKEFILE")
        self.assertIn("rescan", event_kinds(self.watcher.events(timeout=0)))
        mutate("overwrite", self.path)
        self.assertIn("content", event_kinds(self.watcher.events()))
        self.watcher.command("REVOKEDIR")
        self.assertIn("rescan", event_kinds(self.watcher.events(timeout=0)))
        self.watcher.command("LOSSFILE")
        loss = self.watcher.events(timeout=0)
        self.assertIn("error", event_kinds(loss))
        self.assertTrue(any(event[1] == "error" and int(event[4]) == 3
                            for event in loss))
        mutate("overwrite", self.path)
        self.assertIn("content", event_kinds(self.watcher.events()))

        mutate("replace-storm", self.path)
        self.watcher.events(timeout=2500)
        mutate("overwrite", self.path)
        self.assertIn("content", event_kinds(self.watcher.events()))

        mutate("sibling", self.path)
        self.assertEqual(self.watcher.events(timeout=500), [])
        idle = self.watcher.command("IDLE 800")
        counters = next(line for line in idle if line.startswith("IDLE|"))
        _, before_probe, after_probe = counters.split("|")
        self.assertEqual(before_probe, after_probe)
        self.assertEqual(after_probe, "0")

        old_id = self.watcher.subscription_id
        self.watcher.command("UNSUB")
        self.watcher.command("STALE")
        self.assertEqual(self.watcher.events(timeout=0), [])
        mutate("overwrite", self.path)
        self.assertEqual(self.watcher.events(timeout=500), [])
        resubscription = self.watcher.command("RESUB")
        fields = next(line for line in resubscription if line.startswith("SUBSCRIBED|"))
        _, new_id, generation = fields.split("|")
        self.assertNotEqual(int(new_id), old_id)
        mutate("overwrite", self.path)
        events = self.watcher.events()
        self.assertIn("content", event_kinds(events))
        self.assertTrue(all(event[2] == new_id and event[3] == generation
                            for event in events))

    def test_symlink_alias_tracks_target_and_target_replacement(self):
        alias = self.root / "alias source.tpx"
        target_directory = self.root / "target directory"
        target_directory.mkdir()
        target = target_directory / self.path.name
        self.path.rename(target)
        alias.symlink_to(Path(target_directory.name) / target.name)
        self.watcher.close()
        self.watcher = Watcher(self.binary, alias)
        self.watcher.events(timeout=0)

        mutate("overwrite", alias)
        content = self.watcher.events()
        self.assertIn("content", event_kinds(content))
        self.assertTrue(all(event[5] == str(alias) for event in content
                            if event[1] == "content"))
        mutate("replace-target", alias)
        self.assertTrue(set(event_kinds(self.watcher.events())) &
                        {"replaced", "reappeared"})
        mutate("overwrite", alias)
        self.assertIn("content", event_kinds(self.watcher.events()))

    def test_parent_directory_symlink_preserves_lexical_event_path(self):
        parent_alias = self.root / "parent directory alias"
        parent_alias.symlink_to(self.root, target_is_directory=True)
        logical_path = parent_alias / self.path.name
        self.watcher.close()
        self.watcher = Watcher(self.binary, logical_path)
        self.watcher.events(timeout=0)

        mutate("overwrite", logical_path)
        content = self.watcher.events()
        self.assertIn("content", event_kinds(content))
        self.assertTrue(all(event[5] == str(logical_path) for event in content
                            if event[1] == "content"))
        mutate("replace", logical_path)
        self.assertTrue(set(event_kinds(self.watcher.events())) &
                        {"replaced", "reappeared"})

    def test_case_peer_matches_volume_case_policy(self):
        peer = self.path.with_name(
            self.path.name[0].swapcase() + self.path.name[1:])
        same_file = peer.exists() and os.path.samefile(peer, self.path)
        mutate("overwrite-case-peer", self.path)
        observed = event_kinds(self.watcher.events(timeout=700))
        if same_file:
            self.assertIn("content", observed)
        else:
            self.assertEqual(observed, [])

    def test_repeated_start_stop_releases_descriptors_and_worker(self):
        lines = self.watcher.command("CYCLE 100", timeout=20)
        self.assertIn("CYCLED|100", lines)


if __name__ == "__main__":
    unittest.main()
