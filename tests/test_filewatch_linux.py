#!/usr/bin/env python3
"""Headless behavior tests for the Linux inotify backend."""

from pathlib import Path
import os
import selectors
import shutil
import subprocess
import sys
import tempfile
import time
import unittest


ROOT = Path(__file__).resolve().parents[1]
HELPER = ROOT / "tests" / "filewatch_helper.py"


class LineReader:
    def __init__(self, stream):
        self.fd = stream.fileno()
        self.selector = selectors.DefaultSelector()
        self.selector.register(self.fd, selectors.EVENT_READ)
        self.buffer = bytearray()

    def readline(self, timeout=5):
        deadline = time.monotonic() + timeout
        while True:
            newline = self.buffer.find(b"\n")
            if newline >= 0:
                line = bytes(self.buffer[:newline])
                del self.buffer[:newline + 1]
                return line.decode("utf-8")
            remaining = deadline - time.monotonic()
            if remaining <= 0 or not self.selector.select(remaining):
                raise TimeoutError("timed out waiting for process output")
            chunk = os.read(self.fd, 4096)
            if not chunk:
                raise EOFError("process exited before completing its protocol")
            self.buffer.extend(chunk)

    def close(self):
        self.selector.close()


class WatcherClient:
    def __init__(self, binary, path):
        self.process = subprocess.Popen(
            [str(binary), str(path)], stdin=subprocess.PIPE, stdout=subprocess.PIPE,
            stderr=subprocess.PIPE, text=False)
        self.reader = LineReader(self.process.stdout)
        ready = self.reader.readline()
        if not ready.startswith("READY|"):
            raise AssertionError(f"watcher did not become ready: {ready}")
        parts = ready.split("|")
        self.subscription = int(parts[1])
        self.generation = int(parts[2])
        self.status = parts[3]
        if self.status != "ready":
            raise AssertionError(f"watcher startup status was {self.status}")

    def command(self, command, timeout=5):
        self.process.stdin.write((command + "\n").encode())
        self.process.stdin.flush()
        try:
            return self.reader.readline(timeout)
        except EOFError as error:
            code = self.process.wait(timeout=3)
            detail = self.process.stderr.read().decode("utf-8", errors="replace")
            raise AssertionError(
                f"watcher exited with code {code} during {command}; stdout={bytes(self.reader.buffer)!r}: {detail}") from error

    def drain(self, timeout_ms=1500):
        self.process.stdin.write(f"DRAIN|{timeout_ms}\n".encode())
        self.process.stdin.flush()
        lines = []
        while True:
            line = self.reader.readline(max(3, timeout_ms / 1000 + 2))
            if line == "EMPTY":
                return []
            if line == "DRAIN_DONE":
                return [self.parse_event(item) for item in lines]
            lines.append(line)

    @staticmethod
    def parse_event(line):
        parts = line.split("|", 6)
        if len(parts) != 7 or parts[0] != "EVENT":
            raise AssertionError(f"malformed watcher event: {line}")
        return {
            "kind": parts[1], "id": int(parts[2]), "generation": int(parts[3]),
            "status": parts[4], "path": parts[5], "error": parts[6],
        }

    def close(self):
        try:
            if self.process.poll() is None:
                self.process.stdin.write(b"STOP\n")
                self.process.stdin.flush()
                stopped = self.reader.readline(3)
                if stopped != "STOPPED":
                    raise AssertionError(f"watcher stop returned {stopped}")
                self.process.wait(timeout=3)
        finally:
            if self.process.poll() is None:
                self.process.kill()
                self.process.wait(timeout=3)
            self.reader.close()
            self.process.stdin.close()
            self.process.stdout.close()
            self.process.stderr.close()


class LinuxFileWatchTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if not sys.platform.startswith("linux"):
            raise unittest.SkipTest("Linux inotify is required")
        compiler = shutil.which("fpc")
        if not compiler:
            raise RuntimeError("FPC is required for the Linux watcher tests")
        cls.build_directory = tempfile.TemporaryDirectory(prefix="tpx-filewatch-build-")
        output = Path(cls.build_directory.name)
        command = [compiler, "-Crtoi", "-Sa", "-dTPX_FILEWATCH_TESTS",
                   "-Fu" + str(ROOT / "src"),
                   "-FU" + str(output), "-FE" + str(output),
                   "-o" + str(output / "FileWatchLinuxTests"),
                   str(ROOT / "tests" / "FileWatchLinuxTests.lpr")]
        result = subprocess.run(command, cwd=ROOT, capture_output=True, text=True,
                                timeout=30)
        if result.returncode:
            raise RuntimeError(result.stdout + result.stderr)
        cls.binary = output / "FileWatchLinuxTests"

    @classmethod
    def tearDownClass(cls):
        cls.build_directory.cleanup()

    def setUp(self):
        self.directory = tempfile.TemporaryDirectory(prefix="tpx-filewatch-")
        self.root = Path(self.directory.name)
        self.path = self.root / "source.tpx"
        self.path.write_bytes(b"baseline\n")
        self.watcher = WatcherClient(self.binary, self.path)
        self.helper = subprocess.Popen(
            [sys.executable, str(HELPER), str(self.path)], stdin=subprocess.PIPE,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=False)
        self.helper_reader = LineReader(self.helper.stdout)
        self.assertEqual(self.helper_reader.readline(), "READY")

    def tearDown(self):
        if self.helper.poll() is None:
            try:
                self.helper.stdin.write(b"STOP\n")
                self.helper.stdin.flush()
                self.assertEqual(self.helper_reader.readline(3), "STOPPED")
                self.helper.wait(timeout=3)
            finally:
                if self.helper.poll() is None:
                    self.helper.kill()
                    self.helper.wait(timeout=3)
        self.helper_reader.close()
        self.helper.stdin.close()
        self.helper.stdout.close()
        self.helper.stderr.close()
        self.watcher.close()
        self.directory.cleanup()

    def mutate(self, command):
        self.helper.stdin.write((command + "\n").encode())
        self.helper.stdin.flush()
        response = self.helper_reader.readline(5)
        self.assertEqual(response, "ACK " + command)

    def events(self, timeout_ms=1500):
        return self.watcher.drain(timeout_ms)

    def assert_event(self, events, kind, subscription=None, generation=None,
                     expected_path=None):
        matching = [event for event in events if event["kind"] == kind]
        if subscription is not None:
            matching = [event for event in matching if event["id"] == subscription]
        if generation is not None:
            matching = [event for event in matching
                        if event["generation"] == generation]
        self.assertTrue(matching, f"expected {kind} event in {events}")
        for event in matching:
            if event["id"] != 0:
                self.assertEqual(event["path"],
                                 str(expected_path or self.path))

    def test_real_changes_survive_atomic_replacement_and_filter_siblings(self):
        self.mutate("overwrite")
        self.assert_event(self.events(), "content", self.watcher.subscription, 41)
        self.mutate("truncate")
        self.assert_event(self.events(), "content", self.watcher.subscription, 41)
        self.mutate("atomic")
        self.assert_event(self.events(), "replaced", self.watcher.subscription, 41)
        self.mutate("atomic2")
        self.assert_event(self.events(), "replaced", self.watcher.subscription, 41)
        self.mutate("delete")
        self.assert_event(self.events(), "disappeared", self.watcher.subscription, 41)
        self.mutate("create")
        self.assert_event(self.events(), "reappeared", self.watcher.subscription, 41)
        self.mutate("away_back")
        kinds = {event["kind"] for event in self.events()}
        self.assertIn("disappeared", kinds)
        self.assertIn("reappeared", kinds)
        self.mutate("sibling")
        self.assertEqual(self.events(300), [])

    def test_same_size_restored_mtime_still_emits_and_subscriptions_keep_generations(self):
        self.mutate("same_size_mtime")
        self.assert_event(self.events(), "content", self.watcher.subscription, 41)
        response = self.watcher.command("SUB|42")
        _, status, second_id, generation = response.split("|")
        self.assertEqual((status, generation), ("ready", "42"))
        second_id = int(second_id)
        self.mutate("atomic")
        self.assertEqual(self.watcher.command("WAIT|3000"), "PENDING")
        self.assertEqual(self.watcher.command(f"UNSUB|{self.watcher.subscription}"),
                         f"UNSUB|{self.watcher.subscription}")
        self.assertEqual(self.watcher.command(f"UNSUB|{second_id}"), f"UNSUB|{second_id}")
        response = self.watcher.command("SUB|52")
        _, status, third_id, generation = response.split("|")
        self.assertEqual(status, "ready")
        third_id = int(third_id)
        events = self.events()
        self.assert_event(events, "replaced", self.watcher.subscription, 41)
        self.assert_event(events, "replaced", second_id, 42)
        self.assertFalse(any(event["id"] == third_id for event in events),
                         "pending old-generation events were retagged to a new subscription")
        self.mutate("atomic2")
        events = self.events()
        self.assert_event(events, "replaced", third_id, 52)
        self.assertFalse(any(event["id"] in (self.watcher.subscription, second_id)
                             for event in events))

    def test_edit_between_subscribe_and_baseline_remains_queued(self):
        response = self.watcher.command("SUB|77")
        _, status, subscription, generation = response.split("|")
        self.assertEqual((status, generation), ("ready", "77"))
        subscription = int(subscription)
        self.mutate("atomic")
        self.assertEqual(self.watcher.command("WAIT|3000"), "PENDING")
        baseline = self.path.read_bytes()
        self.assertEqual(baseline, b"atomic content\n")
        events = self.events()
        self.assert_event(events, "replaced", subscription, 77)

    def test_injected_malformed_overflow_watch_loss_and_bounded_queue(self):
        self.assertEqual(self.watcher.command("INJECT_BAD"), "INJECTED")
        self.assert_event(self.events(), "rescan")
        self.assertEqual(self.watcher.command("INJECT_OVERFLOW"), "INJECTED")
        overflow_events = self.events()
        self.assertEqual([event["kind"] for event in overflow_events], ["rescan"])
        self.assertEqual(self.watcher.command("INJECT_QUEUE_OVERFLOW"), "INJECTED")
        queue_events = self.events()
        self.assertEqual([event["kind"] for event in queue_events], ["rescan"])
        self.assertEqual(self.watcher.command(
            f"INJECT_LOSS|{self.watcher.subscription}"), "INJECTED")
        self.assert_event(self.events(), "rescan")
        self.mutate("atomic")
        self.assert_event(self.events(), "replaced", self.watcher.subscription, 41)

    def test_missing_parent_is_degraded_and_idle_wait_has_no_recurring_wakeups(self):
        missing = self.root / "absent-parent" / "source.tpx"
        response = self.watcher.command(f"SUBPATH|88|{missing}")
        _, status, subscription, generation = response.split("|")
        self.assertEqual((status, generation), ("degraded", "88"))
        self.assertNotEqual(int(subscription), 0)
        errors = self.events()
        self.assert_event(errors, "error", int(subscription), 88, missing)
        error_text = next(event["error"] for event in errors
                          if event["kind"] == "error")
        self.assertNotIn("Success", error_text,
                         "libc errno was lost while reporting an unwatchable parent")
        first = int(self.watcher.command("POLL_COUNT").split("|")[1])
        time.sleep(0.35)
        second = int(self.watcher.command("POLL_COUNT").split("|")[1])
        self.assertEqual(second, first, "idle watcher returned from poll without an event")

    def test_empty_path_is_rejected(self):
        response = self.watcher.command("SUBPATH|88|")
        self.assertEqual(response, "SUB|error|0|88")

    def test_restart_releases_descriptors_and_stop_wakes_blocking_consumer(self):
        fd_directory = Path(f"/proc/{self.watcher.process.pid}/fd")
        before = len(list(fd_directory.iterdir()))
        self.assertEqual(self.watcher.command("RESTARTS|12"), "RESTARTED|12")
        after = len(list(fd_directory.iterdir()))
        self.assertEqual(after, before, "repeated start/stop leaked file descriptors")
        self.assertEqual(self.watcher.command("STOP_WAIT"), "STOPPED|waiter-awake")
        self.watcher.process.wait(timeout=3)

    def test_close_with_event_pending_stops_without_hanging(self):
        self.mutate("atomic")
        self.helper.stdin.write(b"STOP\n")
        self.helper.stdin.flush()
        self.assertEqual(self.helper_reader.readline(3), "STOPPED")
        self.helper.wait(timeout=3)
        self.assertEqual(self.watcher.command("STOP"), "STOPPED")
        self.watcher.process.wait(timeout=3)


if __name__ == "__main__":
    unittest.main()
