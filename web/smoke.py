#!/usr/bin/env python3
"""Serve web/dist and report the browser state of TpX (status, frames, DOM, shot)."""
import subprocess, sys, time, tempfile
from playwright.sync_api import sync_playwright

dist = sys.argv[1] if len(sys.argv) > 1 else 'web/dist'
shot = sys.argv[2] if len(sys.argv) > 2 else '/tmp/tpx-web-smoke.png'
wait = int(sys.argv[3]) if len(sys.argv) > 3 else 8000
port = 8791
srv = subprocess.Popen([sys.executable, '-m', 'http.server', str(port), '--bind', '127.0.0.1',
                       '--directory', dist], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
time.sleep(0.8)
try:
    with sync_playwright() as p:
        b = p.chromium.launch(executable_path='/usr/bin/chromium', args=['--no-sandbox'])
        pg = b.new_page(viewport={'width': 1200, 'height': 900})
        logs = []
        pg.on('console', lambda m: logs.append(f'{m.type}: {m.text}'))
        pg.on('pageerror', lambda e: logs.append(f'pageerror: {e}'))
        pg.goto(f'http://127.0.0.1:{port}/')
        pg.wait_for_timeout(wait)
        print('status:', pg.inner_text('#status'))
        print('frames:', pg.get_attribute('#lcl', 'data-frames'))
        print('dom children:', pg.eval_on_selector('#lcl-dom', 'e => e.children.length'))
        pg.screenshot(path=shot, full_page=True)
        for l in logs[-15:]:
            print(l[:200])
        b.close()
finally:
    srv.terminate()
