#!/usr/bin/env python3
"""Build and run the strict, display-free Pascal core suite."""
import argparse
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys


ROOT = Path(__file__).resolve().parents[1]
CORE_SOURCE = ROOT / "tests" / "core" / "CoreTests.lpr"
FIXTURES = ROOT / "tests" / "core" / "fixtures"
SUITE_CONFIG = json.loads(
    (ROOT / "tests" / "core" / "suite.json").read_text(encoding="utf-8"))
EXPECTED_CASES = SUITE_CONFIG["expected_cases"]


def run(command, *, cwd, env, timeout):
    return subprocess.run(command, cwd=cwd, env=env, capture_output=True,
                          text=True, encoding="utf-8", errors="replace",
                          timeout=timeout)


def require(condition, message):
    if not condition:
        raise RuntimeError(message)


def build(compiler, build_dir):
    unit_dir = build_dir / "units"
    unit_dir.mkdir(parents=True, exist_ok=True)
    binary = build_dir / ("CoreTests.exe" if os.name == "nt" else "CoreTests")
    command = [compiler, "-B", "-Mdelphi", "-gl", "-Crtoi", "-Sa",
               f"-Fu{ROOT / 'src'}", f"-Fu{ROOT / 'src' / 'lib' / 'XML'}",
               f"-Fu{ROOT / 'tests' / 'core'}", f"-FU{unit_dir}",
               f"-FE{build_dir}", f"-o{binary}", str(CORE_SOURCE)]
    result = run(command, cwd=ROOT, env=os.environ.copy(), timeout=120)
    if result.returncode:
        raise RuntimeError("core test compile failed\n" + result.stdout + result.stderr)
    return binary


def isolated_environment(build_dir):
    run_dir = build_dir / "run"
    empty_path = run_dir / "empty-bin"
    work_dir = run_dir / "work"
    for directory in (empty_path, work_dir, run_dir / "home"):
        directory.mkdir(parents=True, exist_ok=True)
    env = os.environ.copy()
    for name in ("DISPLAY", "WAYLAND_DISPLAY", "TEXINPUTS", "TEXMFHOME",
                 "TEXMFCNF", "LATEX", "PDFLATEX", "DVIPNG", "DVIPS"):
        env.pop(name, None)
    env.update(PATH=str(empty_path), TPX_CORE_FIXTURE_DIR=str(FIXTURES),
               TMPDIR=str(run_dir), TMP=str(run_dir), TEMP=str(run_dir),
               HOME=str(run_dir / "home"), USERPROFILE=str(run_dir / "home"))
    return run_dir, work_dir, env


def load_report(path):
    require(path.is_file(), f"core runner did not write machine report: {path}")
    return json.loads(path.read_text(encoding="utf-8"))


def run_self_check(binary, mode, expected_message, run_dir, work_dir, env):
    report_path = run_dir / f"self-check-{mode}.json"
    result = run([str(binary), "--expected=1", f"--self-test={mode}",
                  f"--report={report_path}"], cwd=work_dir, env=env, timeout=15)
    report = load_report(report_path)
    diagnostic = result.stdout + result.stderr
    require(result.returncode == 1,
            f"runner self-check {mode} did not fail with exit 1\n{diagnostic}")
    require("FAIL self-test-" in diagnostic and expected_message in diagnostic,
            f"runner self-check {mode} lacked a useful failure diagnostic\n{diagnostic}")
    require(report["failed"] == 1 and report["cases"][0]["status"] == "failed",
            f"runner self-check {mode} failure was not retained in its report")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--compiler", default=os.environ.get("FPC") or shutil.which("fpc"))
    parser.add_argument("--build-dir", type=Path, default=ROOT / "obj" / "core-tests")
    parser.add_argument("--filter", default="", help="case-name substring")
    args = parser.parse_args()
    require(args.compiler, "FPC is required to build the strict core suite")
    args.build_dir = args.build_dir.resolve()
    args.build_dir.mkdir(parents=True, exist_ok=True)

    binary = build(args.compiler, args.build_dir)
    run_dir, work_dir, env = isolated_environment(args.build_dir)
    report_path = args.build_dir / "core-report.json"
    report_path.unlink(missing_ok=True)
    command = [str(binary), f"--expected={EXPECTED_CASES}",
               f"--report={report_path}"]
    if args.filter:
        command.append(f"--filter={args.filter}")
    result = run(command, cwd=work_dir, env=env, timeout=20)
    diagnostic = result.stdout + result.stderr
    report = load_report(report_path)
    require(report["suite_count"] == SUITE_CONFIG["expected_suites"] and
            report["discovered"] == EXPECTED_CASES,
            f"expected {SUITE_CONFIG['expected_suites']} core suite(s) and "
            f"{EXPECTED_CASES} discovered cases; got {report}")
    require(report["selected"] > 0, "strict core run selected no cases")
    require(report["failed"] == 0 and result.returncode == 0,
            "strict core suite failed\n" + diagnostic)
    require(report["passed"] == report["selected"],
            f"strict core report has inconsistent counts: {report}")

    run_self_check(binary, "wrong-geometry",
                   "fixture line length (intentional wrong expectation)",
                   run_dir, work_dir, env)
    run_self_check(binary, "scenario-failure",
                   "intentional scenario failure for runner self-check",
                   run_dir, work_dir, env)
    print(diagnostic, end="")
    print("CORE_RUNNER_SELF_CHECKS passed=2")
    print("CORE_REPORT=" + str(report_path))
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (OSError, RuntimeError, subprocess.TimeoutExpired, json.JSONDecodeError) as error:
        print(f"CORE TEST DRIVER FAILED: {error}", file=sys.stderr)
        raise SystemExit(1)
