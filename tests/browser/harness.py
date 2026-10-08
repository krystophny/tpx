#!/usr/bin/env python3
"""Screenshot-driven behavioral playtest of the browser build of TpX.

The oracle is the real rendered output: canvas pixels, frame counter, DOM
children and the application title - not just "the page loaded". Every action
writes a numbered screenshot and the run writes a manifest.

  tests/browser/playtest.py [dist] [outdir] [--only name1,name2]

Exit status is non-zero when a scenario fails.
"""
import json
import os
import subprocess
import sys
import time
from pathlib import Path

from playwright.sync_api import sync_playwright

ROOT = Path(__file__).resolve().parents[2]
DIST = ROOT / 'web/dist'
OUT = ROOT / 'dist/browser-playtest'
PORT = int(os.environ.get('TPX_WEB_PORT', '8795'))
CHROMIUM = os.environ.get('CHROMIUM', '/usr/bin/chromium')


class App:
    """Drive the browser TpX and observe it through its rendered output."""

    def __init__(self, page, prefix=''):
        self.page = page
        self.prefix = prefix
        self.shots = 0
        self.errors = []
        page.on('pageerror', lambda e: self.errors.append(str(e)[:400]))
        page.on('console', lambda m: self.errors.append(f'{m.type}: {m.text}'[:400])
                if m.type == 'error' else None)

    # --- observation -------------------------------------------------------
    def box(self):
        return self.page.evaluate('''() => { const c=document.querySelector('#lcl');
          const r=c.getBoundingClientRect();
          return {x:r.x,y:r.y,w:r.width,h:r.height,nw:c.width,nh:c.height}; }''')

    def frames(self):
        return int(self.page.get_attribute('#lcl', 'data-frames') or 0)

    def pixels(self, box=None):
        """Return the canvas pixels of a client-space region as bytes."""
        b = box or self.box()
        return self.page.evaluate('''(r) => {
          const c=document.querySelector('#lcl');
          return Array.from(c.getContext('2d').getImageData(r.x, r.y, r.w, r.h).data);
        }''', {'x': int(b[0]), 'y': int(b[1]), 'w': int(b[2]), 'h': int(b[3])})

    def changed(self, before, box=None):
        after = self.pixels(box)
        diff = sum(1 for i in range(0, len(after), 4)
                   if abs(after[i] - before[i]) > 8 or abs(after[i+1] - before[i+1]) > 8
                   or abs(after[i+2] - before[i+2]) > 8)
        return diff, after

    def painted(self, old, timeout=4000):
        self.page.wait_for_function('(old) => Number(document.querySelector("#lcl").dataset.frames) > old',
                                     arg=old, timeout=timeout)

    def viewport(self):
        """Measured client-space box of the white drawing paper.

        The viewport is created at runtime (alClient inside Panel1), so its box
        depends on the current layout; scanning the presented frame keeps the
        scenarios correct after a resize instead of hard-coding LFM numbers.
        """
        return self.page.evaluate('''() => {
          const c = document.querySelector('#lcl');
          const g = c.getContext('2d');
          const w = c.width, h = c.height;
          const mid = g.getImageData(0, Math.floor(h/2), w, 1).data;
          const white = i => mid[i] > 250 && mid[i+1] > 250 && mid[i+2] > 250;
          let best = [0,0], run = -1;
          for (let x = 0; x < w; x++) {
            if (white(x*4)) { if (run < 0) run = x; }
            else if (run >= 0) { if (x-run > best[1]-best[0]) best = [run, x]; run = -1; }
          }
          if (run >= 0 && w-run > best[1]-best[0]) best = [run, w];
          const cx = Math.floor((best[0]+best[1])/2);
          const col = g.getImageData(cx, 0, 1, h).data;
          const whiteY = i => col[i] > 250 && col[i+1] > 250 && col[i+2] > 250;
          let y0 = 0, y1 = h, y = 0;
          while (y < h && !whiteY(y*4)) y++;
          y0 = y; while (y < h && whiteY(y*4)) y++; y1 = y;
          return {x: best[0], y: y0, w: best[1]-best[0], h: Math.max(0, y1-y0)};
        }''')

    def center(self):
        v = self.viewport()
        return v['x'] + v['w'] // 2, v['y'] + v['h'] // 2

    def dom_count(self):
        return self.page.evaluate('''() => { const d=document.querySelector('#lcl-dom');
          return d ? d.children.length : -1; }''')

    def title(self):
        return self.page.title()

    # --- action ----------------------------------------------------------
    def shot(self, name):
        self.shots += 1
        path = OUT / f'{self.shots:02d}-{self.prefix}-{name}.png'
        self.page.screenshot(path=str(path))
        return path.name

    def at(self, x, y):
        """Client-space canvas point -> page point."""
        b = self.box()
        return b['x'] + x * b['w'] / b['nw'], b['y'] + y * b['h'] / b['nh']

    def click(self, x, y, button='left', name=None):
        px, py = self.at(x, y)
        self.page.mouse.click(px, py, button=button)
        if name:
            self.shot(name)

    def drag(self, x0, y0, x1, y1, name=None, steps=12):
        px, py = self.at(x0, y0)
        self.page.mouse.move(px, py)
        self.page.mouse.down()
        for i in range(1, steps + 1):
            self.page.mouse.move(px + (x1 - x0) * i / steps, py + (y1 - y0) * i / steps)
        self.page.mouse.up()
        if name:
            self.shot(name)

    def move(self, x, y):
        px, py = self.at(x, y)
        self.page.mouse.move(px, py)

    def wheel(self, x, y, delta):
        px, py = self.at(x, y)
        self.page.mouse.move(px, py)
        self.page.mouse.wheel(0, delta)

    def key(self, chord, name=None):
        self.page.keyboard.press(chord)
        if name:
            self.shot(name)

    def type(self, text):
        self.page.keyboard.type(text)
