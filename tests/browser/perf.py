#!/usr/bin/env python3
"""Performance gates for the browser (WASI) build.

Measures the four contract numbers in a real Chromium:

  startup   first presented frame after navigation
  idle      frames and renderer CPU with no input at all
  pointer   renderer CPU during a continuous ~120 Hz pointer move
  payload   tpx.wasm size on disk and gzipped

Prints a PASS/FAIL line per gate and exits non-zero on any miss. Run after
`make web`. Requires chromium and playwright.

  TMPDIR=/path/with/space python3 tests/browser/perf.py [--port 8137]
"""

import argparse
import gzip
import os
import subprocess
import sys
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from harness import App  # noqa: E402

ROOT = Path(__file__).resolve().parents[2]

MAX_IDLE_FRAMES = 1          # nothing is dirty, so nothing may repaint
MAX_IDLE_CPU = 1.0           # percent of one core, averaged over the window
MAX_POINTER_CPU = 10.0       # percent of one core at continuous pointer move
MAX_STARTUP_MS = 2000.0
MAX_WASM_GZIP_MB = 10.0


def renderer_cpu(pids):
    """Total CPU seconds across the Chromium renderer processes."""
    total = 0.0
    ticks = os.sysconf('SC_CLK_TCK')
    for pid in pids:
        try:
            with open(f'/proc/{pid}/stat', 'rb') as fh:
                fields = fh.read().decode('ascii', 'replace').split()
            total += (int(fields[13]) + int(fields[14])) / ticks
        except (OSError, IndexError, ValueError):
            pass
    return total


def chromium_pids(kind):
    """Pids of running chromium processes of the given --type= kind."""
    processes = {}
    for entry in Path('/proc').iterdir():
        if not entry.name.isdigit():
            continue
        try:
            fields = (entry / 'stat').read_text().rsplit(')', 1)[1].split()
            cmd = (entry / 'cmdline').read_bytes().decode('ascii', 'replace')
            processes[int(entry.name)] = (int(fields[1]), cmd)
        except (OSError, ValueError, IndexError):
            continue
    def owned(pid):
        seen = set()
        while pid in processes and pid not in seen:
            seen.add(pid)
            pid = processes[pid][0]
            if pid == os.getpid():
                return True
        return False
    return [str(pid) for pid, (_, cmd) in processes.items()
            if f'--type={kind}' in cmd and owned(pid)]


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--port', type=int, default=8137)
    ap.add_argument('--idle', type=float, default=12.0)
    ap.add_argument('--pointer', type=float, default=6.0)
    args = ap.parse_args()

    wasm = ROOT / 'web' / 'dist' / 'tpx.wasm'
    if not wasm.exists():
        sys.exit(f'{wasm} missing - run `make web` first')

    raw = wasm.stat().st_size
    # The dev server serves the raw module; the gate is the transfer size, so
    # compress it the way a production server would ( gzip -9 ).
    gz = len(gzip.compress(wasm.read_bytes(), 9))

    srv = subprocess.Popen(
        [sys.executable, '-m', 'http.server', str(args.port), '--bind', '127.0.0.1',
         '--directory', str(ROOT / 'web' / 'dist')],
        stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    time.sleep(0.8)

    from playwright.sync_api import sync_playwright

    results = []
    try:
        with sync_playwright() as p:
            browser = p.chromium.launch(executable_path=os.environ.get('CHROMIUM', '/usr/bin/chromium'),
                                        args=['--no-sandbox'])
            page = browser.new_page(viewport={'width': 1280, 'height': 900})
            app = App(page, 'perf')

            nav_start = time.monotonic()
            page.goto(f'http://127.0.0.1:{args.port}/')
            page.wait_for_selector('#status[data-state=ready]', timeout=30000)
            page.wait_for_function(
                "() => Number(document.querySelector('#lcl').dataset.frames||0) > 0",
                timeout=30000)
            startup_ms = (time.monotonic() - nav_start) * 1000.0

            # Idle: no input at all, frame counter must not move.
            page.wait_for_timeout(500)
            f0, cpu0 = app.frames(), renderer_cpu(chromium_pids('renderer'))
            t0 = time.monotonic()
            page.wait_for_timeout(args.idle * 1000)
            idle_wall = time.monotonic() - t0
            f1, cpu1 = app.frames(), renderer_cpu(chromium_pids('renderer'))
            idle_cpu = 100.0 * (cpu1 - cpu0) / idle_wall

            # Pointer: continuous ~120 Hz move inside the paper.
            v = app.viewport()
            cx, cy = v['x'] + v['w'] // 2, v['y'] + v['h'] // 2
            pids = chromium_pids('renderer')
            cpu_a = renderer_cpu(pids)
            t1 = time.monotonic()
            bounds = app.box()
            page.evaluate("""async ({x, y, ms}) => {
              const canvas = document.querySelector('#lcl');
              let i = 0;
              await new Promise(resolve => {
                const timer = setInterval(() => {
                  canvas.dispatchEvent(new PointerEvent('pointermove', {
                    clientX: x + 120*((i%40)-20)/20,
                    clientY: y + 80*((Math.floor(i/5)%16)-8)/8,
                    pointerId: 1, pointerType: 'mouse', bubbles: true
                  }));
                  i++;
                }, 1000/120);
                setTimeout(() => { clearInterval(timer); resolve(); }, ms);
              });
            }""", {'x': bounds['x'] + cx, 'y': bounds['y'] + cy,
                    'ms': args.pointer*1000})
            cpu_b = renderer_cpu(pids)
            pointer_cpu = 100.0 * (cpu_b - cpu_a) / (time.monotonic() - t1)
            moved_frames = app.frames() - f1

            # Park off canvas so the next measurement starts clean.
            b = app.box()
            page.mouse.move(b['x'] + b['w'] / 2, max(2.0, b['y'] - 20))
            page.wait_for_timeout(400)
            idle_after_cpu0 = renderer_cpu(chromium_pids('renderer'))
            page.wait_for_timeout(3000)
            idle_after_cpu = 100.0 * (renderer_cpu(chromium_pids('renderer'))
                                       - idle_after_cpu0) / 3.0

            browser.close()

            results = [
                ('startup first frame', startup_ms, MAX_STARTUP_MS, 'ms'),
                ('idle frames in %.0fs' % args.idle, f1 - f0, MAX_IDLE_FRAMES, 'frames'),
                ('idle cpu', idle_cpu, MAX_IDLE_CPU, '% core'),
                ('pointer cpu (%.0fs @120Hz)' % args.pointer, pointer_cpu,
                 MAX_POINTER_CPU, '% core'),
                ('idle cpu after activity', idle_after_cpu, MAX_IDLE_CPU, '% core'),
                ('wasm gzipped', gz / 1048576.0, MAX_WASM_GZIP_MB, 'MB'),
            ]
            print(f'raw wasm {raw} bytes, gzipped {gz} bytes '
                  f'({100.0 * gz / raw:.1f}%)')
            print(f'pointer frames moved: {moved_frames}')
    finally:
        srv.terminate()

    failed = 0
    for name, got, limit, unit in results:
        ok = got <= limit
        failed += 0 if ok else 1
        print(f'{"PASS" if ok else "FAIL"}  {name:28s} {got:8.2f} {unit}  (limit {limit})')
    print(f'{len(results) - failed}/{len(results)} performance gates passed')
    return 1 if failed else 0


if __name__ == '__main__':
    sys.exit(main())
