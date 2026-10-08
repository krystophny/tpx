#!/usr/bin/env python3
"""Report the browser TpX layout as text: canvas box and a coarse color grid.

Avoids dumping screenshots into the conversation while still giving the
coordinates the scenario harness needs.
"""
import subprocess, sys, time, json
from playwright.sync_api import sync_playwright

DIST = sys.argv[1] if len(sys.argv) > 1 else 'web/dist'
PORT = 8793
srv = subprocess.Popen([sys.executable, '-m', 'http.server', str(PORT), '--bind', '127.0.0.1',
                       '--directory', DIST], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
time.sleep(0.8)
try:
    with sync_playwright() as p:
        b = p.chromium.launch(executable_path='/usr/bin/chromium', args=['--no-sandbox'])
        pg = b.new_page(viewport={'width': 1280, 'height': 900})
        errs = []
        pg.on('pageerror', lambda e: errs.append(str(e)[:200]))
        pg.goto(f'http://127.0.0.1:{PORT}/')
        pg.wait_for_selector('#status[data-state=ready]', timeout=30000)
        pg.wait_for_function('Number(document.querySelector("#lcl").dataset.frames)>0')
        box = pg.evaluate('''() => { const c=document.querySelector('#lcl'); const r=c.getBoundingClientRect();
          return {x:r.x,y:r.y,w:r.width,h:r.height,nw:c.width,nh:c.height}; }''')
        print('canvas', json.dumps(box))
        # coarse grid: dominant color class per cell, read from the canvas backing store
        grid = pg.evaluate('''() => {
          const c=document.querySelector('#lcl'), g=c.getContext('2d');
          const cols=24, rows=14, cw=c.width/cols, ch=c.height/rows, out=[];
          const key=(r,g,b)=>{ if(r>240&&g>240&&b>240) return '.';
            if(r<80&&g<80&&b<80) return '#';
            if(r>180&&g<110&&b<110) return 'R'; if(r<120&&g>140&&b>180) return 'B';
            if(r>180&&g>160&&b<140) return 'Y'; if(r>140&&g<140&&b>180) return 'P';
            if(g>140&&r<160&&b<160) return 'G'; return '+'; };
          for (let ry=0; ry<rows; ry++){ let line='';
            for (let rx=0; rx<cols; rx++){
              const x=Math.floor(rx*cw), y=Math.floor(ry*ch);
              const d=g.getImageData(x,y,1,1).data; line+=key(d[0],d[1],d[2]); }
            out.push(line); }
          return out; }''')
        print('\n'.join(grid))
        print('frames', pg.get_attribute('#lcl', 'data-frames'), 'title', json.dumps(pg.title()))
        print('errors', errs)
        b.close()
finally:
    srv.terminate()
