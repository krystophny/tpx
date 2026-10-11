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
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from harness import App  # noqa: E402
from scenarios import SCENARIOS  # noqa: E402
from source_format_scenarios import SOURCE_FORMAT_SCENARIOS  # noqa: E402
from recovery_scenarios import SCENARIOS as RECOVERY_SCENARIOS  # noqa: E402

from playwright.sync_api import sync_playwright  # noqa: E402

ROOT = Path(__file__).resolve().parents[2]
SCENARIOS = SCENARIOS + SOURCE_FORMAT_SCENARIOS

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
SCENARIOS = SCENARIOS + RECOVERY_SCENARIOS
RESTART_SCENARIOS = {'recovery-restart-geometry', 'recovery-restart-raw-label',
                     'recovery-source-assets'}

class QuietHandler(SimpleHTTPRequestHandler):
    def log_message(self, *args):
        pass

srv = ThreadingHTTPServer(('127.0.0.1', args.port), partial(QuietHandler, directory=args.dist))
Thread(target=srv.serve_forever, daemon=True).start()
results, failures = [], []
try:
    with sync_playwright() as p:
        browser_type = getattr(p, args.browser)
        launch_options = ({'executable_path': args.chromium, 'args': ['--no-sandbox']}
                          if args.browser == 'chromium' else {})
        browser = browser_type.launch(**launch_options)
        for name, fn in SCENARIOS:
            if wanted and name not in wanted:
                continue
            profile = None
            context = None
            restart_factory = None
            if name in RESTART_SCENARIOS:
                profile = tempfile.mkdtemp(prefix='tpx-browser-recovery-')
                restart_factory = lambda: browser_type.launch_persistent_context(
                    profile, viewport={'width': 1280, 'height': 900}, **launch_options)
                context = restart_factory()
                page = context.pages[0] if context.pages else context.new_page()
            else:
                context = browser.new_context(viewport={'width': 1280, 'height': 900})
                page = context.new_page()
            app = App(page, name, OUT, context, restart_factory)
            status = 'pass'
            detail = ''
            try:
                app.url = f'http://127.0.0.1:{srv.server_port}/'
                page.goto(app.url)
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
                status = 'fail'
                detail = f'browser error: {app.errors[0][:160]}'
            if status != 'pass':
                failures.append(name)
            if args.screenshots or status != 'pass':
                try:
                    app.shot(status)
                except Exception as exc:  # noqa: BLE001 - browser may have exited
                    detail = (f'{detail}; screenshot unavailable: {type(exc).__name__}')[:220]
            try:
                frame_count = app.frames()
            except Exception:  # noqa: BLE001 - target may have crashed
                frame_count = None
            results.append({'scenario': name, 'status': status, 'detail': detail,
                            'frames': frame_count, 'shots': app.shots})
            print(f'{status:5} {name:24} {detail[:150]}')
            cleanup_page = app.page
            cleanup_context = app.context
            try:
                if not cleanup_page.is_closed():
                    cleanup_page.on('dialog', lambda dialog: dialog.accept()
                                    if dialog.type == 'beforeunload' else dialog.dismiss())
            except Exception:  # noqa: BLE001 - cleanup must survive a browser exit
                pass
            try:
                if cleanup_context is not None:
                    cleanup_context.close()
            except Exception:  # noqa: BLE001 - context may already have exited
                pass
            if profile is not None:
                import shutil
                shutil.rmtree(profile, ignore_errors=True)
        try:
            browser.close()
        except Exception:  # noqa: BLE001 - a scenario can crash the browser process
            pass
finally:
    srv.shutdown()
    srv.server_close()

manifest = {'passed': len(results) - len(failures), 'failed': len(failures),
            'failures': failures, 'results': results}
(OUT / 'manifest.json').write_text(json.dumps(manifest, indent=1))
print(f'\n{manifest["passed"]}/{len(results)} passed, screenshots in {OUT}')
sys.exit(1 if failures else 0)
