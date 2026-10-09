#!/usr/bin/env python3
"""Run the browser playtest scenarios and write a manifest.

  tests/browser/playtest.py [dist] [outdir] [--only name,name] [--screenshots]

Each scenario gets a freshly loaded application so that failures cannot cascade
into the next case. Prints one line per scenario and exits non-zero on failure.
"""
import argparse
import os
import json
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer
from functools import partial
from threading import Thread
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from harness import App  # noqa: E402
from scenarios import SCENARIOS  # noqa: E402

from playwright.sync_api import sync_playwright  # noqa: E402

ROOT = Path(__file__).resolve().parents[2]

parser = argparse.ArgumentParser()
parser.add_argument('dist', nargs='?', default=str(ROOT / 'web/dist'))
parser.add_argument('outdir', nargs='?', default=str(ROOT / 'dist/browser-playtest'))
parser.add_argument('--only', default='')
parser.add_argument('--screenshots', action='store_true')
parser.add_argument('--port', type=int, default=0)
parser.add_argument('--browser', choices=['chromium', 'firefox', 'webkit'], default='chromium')
parser.add_argument('--chromium', default=os.environ.get('CHROMIUM', '/usr/bin/chromium'))
args = parser.parse_args()

wanted = set(n for n in args.only.split(',') if n)
OUT = Path(args.outdir)
OUT.mkdir(parents=True, exist_ok=True)

class QuietHandler(SimpleHTTPRequestHandler):
    def log_message(self, *args):
        pass

srv = ThreadingHTTPServer(('127.0.0.1', args.port), partial(QuietHandler, directory=args.dist))
Thread(target=srv.serve_forever, daemon=True).start()
results, failures = [], []
try:
    with sync_playwright() as p:
        browser = (p.chromium.launch(executable_path=args.chromium, args=['--no-sandbox'])
                   if args.browser == 'chromium' else getattr(p, args.browser).launch())
        for name, fn in SCENARIOS:
            if wanted and name not in wanted:
                continue
            page = browser.new_page(viewport={'width': 1280, 'height': 900})
            app = App(page, name, OUT)
            status = 'pass'
            detail = ''
            try:
                page.goto(f'http://127.0.0.1:{srv.server_port}/')
                page.wait_for_selector('#status[data-state=ready]', timeout=30000)
                page.wait_for_function('Number(document.querySelector("#lcl").dataset.frames)>0')
                page.wait_for_timeout(500)
                problem = fn(app)
                # Scenarios raise on failure and describe success, so a returned
                # string is the evidence, not a problem.
                detail = problem or ''
                if app.crashed():
                    status, detail = 'fail', f'instance died: {app.errors[0][:150]}'
            except Exception as exc:  # noqa: BLE001 - report, do not swallow silently
                status = 'error'
                detail = f'{type(exc).__name__}: {str(exc)[:180]}'.replace('\n', ' ')
            if app.errors and status == 'pass':
                detail = (detail + ' | ' if detail else '') + f'console: {app.errors[0][:160]}'
            if status != 'pass':
                failures.append(name)
            if args.screenshots or status != 'pass':
                app.shot(status)
            results.append({'scenario': name, 'status': status, 'detail': detail,
                            'frames': app.frames(), 'shots': app.shots})
            print(f'{status:5} {name:24} {detail[:150]}')
            page.close()
        browser.close()
finally:
    srv.shutdown()
    srv.server_close()

manifest = {'passed': len(results) - len(failures), 'failed': len(failures),
            'failures': failures, 'results': results}
(OUT / 'manifest.json').write_text(json.dumps(manifest, indent=1))
print(f'\n{manifest["passed"]}/{len(results)} passed, screenshots in {OUT}')
sys.exit(1 if failures else 0)
