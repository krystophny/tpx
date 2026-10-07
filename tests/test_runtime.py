#!/usr/bin/env python3
"""Run native behavioral scenarios with bounded execution on Linux and macOS."""
import os
from pathlib import Path
import subprocess
import unittest

BINARY = Path(os.environ["RUNTIME_BINARY"]).resolve()
SCENARIOS = ("exit-clean", "exit-no", "exit-cancel", "exit-yes",
             "exit-close", "exit-fallback", "text-selection", "default-view", "toolbar")


class RuntimeTests(unittest.TestCase):
    def test_runtime_scenarios(self):
        for scenario in SCENARIOS:
            with self.subTest(scenario=scenario):
                result = subprocess.run([str(BINARY), scenario],
                                        capture_output=True, text=True, timeout=20)
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                self.assertIn("PASS " + scenario, result.stdout)


if __name__ == "__main__":
    unittest.main()
