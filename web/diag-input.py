#!/usr/bin/env python3
"""Separate host delivery from application behavior.

Counts host-side deliveries of wheel/key calls and reports the visible effect
of each keystroke, so a failing scenario can be classified correctly.
"""
import subprocess, sys, time
sys.path.insert(0, 'tests/browser')
from playwright.sync_api import sync_playwright
from harness import App

PORT = 8813
srv = subprocess.Popen([sys.executable, '-m', 'http.server', str(PORT), '--bind', '127.0.0.1',
                       '--directory', 'web/dist'], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
time.sleep(0.8)


def diff(a, b):
    return sum(1 for i in range(0, len(a), 4)
               if abs(a[i] - b[i]) > 8 or abs(a[i+1] - b[i+1]) > 8 or abs(a[i+2] - b[i+2]) > 8)


try:
    with sync_playwright() as p:
        b = p.chromium.launch(executable_path='/usr/bin/chromium', args=['--no-sandbox'])
        pg = b.new_page(viewport={'width': 1280, 'height': 900})
        a = App(pg, 'keys')
        pg.goto(f'http://127.0.0.1:{PORT}/')
        pg.wait_for_selector('#status[data-state=ready]', timeout=30000)
        pg.wait_for_timeout(1500)
        # Count host deliveries.
        pg.evaluate('''() => { const w = window.lclDemo; const c = {};
          window.__counts = c;
          for (const n of ['lcl_wheel','lcl_key','lcl_pointer','lcl_resize']) {
            const f = w[n].bind(w); w[n] = (...x) => { c[n]=(c[n]||0)+1; return f(...x); }; }
          const canvas = document.querySelector('#lcl');
          c.dom_wheel = 0; canvas.addEventListener('wheel', () => c.dom_wheel++);
          c.dom_keydown = 0; document.addEventListener('keydown', () => c.dom_keydown++); }''')
        v = a.viewport()
        cx, cy = v['x'] + v['w']//2, v['y'] + v['h']//2
        region = [cx-150, cy-120, 300, 240]
        top = [0, 0, v['w'], 140]

        pg.mouse.move(cx, cy)
        pg.mouse.wheel(0, -240); pg.wait_for_timeout(400)
        print('wheel   ', pg.evaluate('() => window.__counts'))
        for chord in ['F10', 'Alt+f', 'Escape', 'Tab', 'Control+a', 'Delete']:
            before_top = a.pixels(top)
            before_mid = a.pixels(region)
            pg.keyboard.press(chord)
            pg.wait_for_timeout(500)
            print(f'{chord:11} topDiff={diff(before_top, a.pixels(top)):6} '
                  f'midDiff={diff(before_mid, a.pixels(region)):6} '
                  f'frames={a.frames()}')
        print('counts ', pg.evaluate('() => window.__counts'))
        # Escape while a drag is still down (real cancel semantics).
        before = a.pixels(region)
        pg.mouse.move(cx-80, cy-50); pg.mouse.down()
        for i in range(6):
            pg.mouse.move(cx-80+i*25, cy-50+i*16); pg.wait_for_timeout(15)
        mid = a.pixels(region)
        pg.keyboard.press('Escape'); pg.wait_for_timeout(300)
        pg.mouse.up(); pg.wait_for_timeout(300)
        print('cancel  dragInk=%d afterEscape=%d' % (diff(before, mid), diff(before, a.pixels(region))))
        b.close()
finally:
    srv.terminate()
