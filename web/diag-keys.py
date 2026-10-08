#!/usr/bin/env python3
"""Diagnostic: do keyboard and wheel events reach Pascal at all?

Uses only visible effects: Tab must move focus (repaint), Ctrl+Z must undo.
Prints changed-pixel counts and the last Pascal console lines.
"""
import subprocess, sys, time
from playwright.sync_api import sync_playwright

DIST = sys.argv[1] if len(sys.argv) > 1 else 'web/dist'
PORT = 8797
srv = subprocess.Popen([sys.executable, '-m', 'http.server', str(PORT), '--bind', '127.0.0.1',
                       '--directory', DIST], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
time.sleep(0.8)


def diff(a, b):
    return sum(1 for i in range(0, len(a), 4)
               if abs(a[i] - b[i]) > 8 or abs(a[i+1] - b[i+1]) > 8 or abs(a[i+2] - b[i+2]) > 8)


try:
    with sync_playwright() as p:
        b = p.chromium.launch(executable_path='/usr/bin/chromium', args=['--no-sandbox'])
        pg = b.new_page(viewport={'width': 1280, 'height': 900})
        logs = []
        pg.on('console', lambda m: logs.append(f'{m.type}: {m.text}'[:160]))
        pg.on('pageerror', lambda e: logs.append('pageerror: ' + str(e)[:200]))
        pg.goto(f'http://127.0.0.1:{PORT}/')
        pg.wait_for_selector('#status[data-state=ready]', timeout=30000)
        pg.wait_for_function('Number(document.querySelector("#lcl").dataset.frames)>0')
        exp = pg.evaluate('() => Object.keys(window.lclDemo)')
        print('exports', [e for e in exp if e.startswith('lcl')])
        px = '''(r) => Array.from(document.querySelector('#lcl').getContext('2d')
                 .getImageData(r[0], r[1], r[2], r[3]).data)'''
        box = [0, 0, 1240, 800]
        base = pg.evaluate(px, box)
        f0 = pg.get_attribute('#lcl', 'data-frames')
        for chord in ['Tab', 'Tab', 'Escape', 'Control+z', 'Delete']:
            pg.keyboard.press(chord)
            pg.wait_for_timeout(400)
            now = pg.evaluate(px, box)
            print(f'{chord:11} frames {f0}->{pg.get_attribute("#lcl","data-frames")} diff {diff(base, now)}')
            base, f0 = now, pg.get_attribute('#lcl', 'data-frames')
        # draw, then wheel over the canvas centre
        c = pg.evaluate('''() => { const r=document.querySelector('#lcl').getBoundingClientRect();
                        return [r.x, r.y, r.width, r.height, document.querySelector('#lcl').width]; }''')
        pg.mouse.click(c[0] + c[2]*0.5, c[1] + c[3]*0.5)
        pg.wait_for_timeout(200)
        before = pg.evaluate(px, box)
        pg.mouse.wheel(0, -240)
        pg.wait_for_timeout(500)
        print('wheel diff', diff(before, pg.evaluate(px, box)), 'frames', pg.get_attribute('#lcl', 'data-frames'))
        print('--- pascal/console tail')
        for l in logs[-8:]:
            print(l)
        b.close()
finally:
    srv.terminate()
