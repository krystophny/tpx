#!/usr/bin/env python3
"""Which toolbar buttons actually create objects? Answered by behaviour.

For every candidate point in the top strips: click it, drag on the measured
paper, keep only points whose drag leaves ink behind and whose Ctrl+Z removes
that ink again. Those are drawing tools with working undo.
"""
import subprocess, sys, time
sys.path.insert(0, 'tests/browser')
from playwright.sync_api import sync_playwright
from harness import App

PORT = 8811
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
        a = App(pg, 'tools')
        pg.goto(f'http://127.0.0.1:{PORT}/')
        pg.wait_for_selector('#status[data-state=ready]', timeout=30000)
        pg.wait_for_timeout(1500)
        v = a.viewport()
        cx, cy = v['x'] + v['w']//2, v['y'] + v['h']//2
        region = [cx-140, cy-110, 280, 220]
        print('paper', v['x'], v['y'], v['w'], v['h'], 'region', region)
        hits = 0
        for y in (8, 40, 64):
            for x in range(10, 760, 24):
                clean = a.pixels(region)
                a.click(x, y)
                pg.wait_for_timeout(120)
                a.drag(cx-70, cy-45, cx+70, cy+45)
                pg.wait_for_timeout(200)
                after = a.pixels(region)
                ink = diff(clean, after)
                if ink < 800:
                    continue
                pg.keyboard.press('Control+z'); pg.wait_for_timeout(300)
                back = diff(clean, a.pixels(region))
                hits += 1
                print(f'x={x:4} y={y:3} ink={ink:6} afterUndo={back:6} {"OK" if back < 300 else "UNDO-BAD"}')
        print('button candidates that painted:', hits)
        b.close()
finally:
    srv.terminate()
