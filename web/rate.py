#!/usr/bin/env python3
"""Frame-rate oracle: sample data-frames over time in distinct phases."""
import subprocess, sys, time
from playwright.sync_api import sync_playwright

DIST = sys.argv[1] if len(sys.argv) > 1 else 'web/dist'
PORT = 8799
srv = subprocess.Popen([sys.executable, '-m', 'http.server', str(PORT), '--bind', '127.0.0.1',
                       '--directory', DIST], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
time.sleep(0.8)


def sample(pg, seconds, label):
    f0 = int(pg.get_attribute('#lcl', 'data-frames') or 0)
    t0 = time.time()
    time.sleep(seconds)
    f1 = int(pg.get_attribute('#lcl', 'data-frames') or 0)
    print(f'{label:22} {f1-f0:4} frames in {seconds:4.1f}s  ({(f1-f0)/seconds:5.1f} fps)')
    return f1


try:
    with sync_playwright() as p:
        b = p.chromium.launch(executable_path='/usr/bin/chromium', args=['--no-sandbox'])
        pg = b.new_page(viewport={'width': 1280, 'height': 900})
        pg.goto(f'http://127.0.0.1:{PORT}/')
        pg.wait_for_selector('#status[data-state=ready]', timeout=30000)
        pg.wait_for_function('Number(document.querySelector("#lcl").dataset.frames)>0')
        c = pg.evaluate('''() => { const r=document.querySelector('#lcl').getBoundingClientRect();
                        return [r.x, r.y, r.width, r.height]; }''')
        sample(pg, 3, 'boot idle')
        pg.mouse.move(c[0] + c[2]*0.4, c[1] + c[3]*0.5)
        pg.mouse.down()
        for i in range(1, 25):
            pg.mouse.move(c[0] + c[2]*(0.4 + 0.004*i), c[1] + c[3]*(0.5 + 0.002*i))
            time.sleep(0.02)
        pg.mouse.up()
        sample(pg, 3, 'after drag idle')
        pg.keyboard.press('Control+z')
        sample(pg, 3, 'after undo idle')
        pg.mouse.move(c[0] + c[2]*0.4, c[1] + c[3]*0.5)
        for i in range(60):
            pg.mouse.move(c[0] + c[2]*0.4 + (i % 20)*4, c[1] + c[3]*0.5 + (i % 13)*3)
            time.sleep(0.01)
        sample(pg, 2, 'during pointer move')
        b.close()
finally:
    srv.terminate()
