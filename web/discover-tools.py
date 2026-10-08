#!/usr/bin/env python3
"""Find the drawing tools by behaviour, not by guessing coordinates.

For each candidate point on the toolbar strip: click it, drag on the paper,
and check whether the drag painted something that Undo removes. A tool that
creates an object answers (drag=changed, undo=restored).
"""
import subprocess, sys, time
from playwright.sync_api import sync_playwright

DIST = sys.argv[1] if len(sys.argv) > 1 else 'web/dist'
PORT = 8801
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
        pg.goto(f'http://127.0.0.1:{PORT}/')
        pg.wait_for_selector('#status[data-state=ready]', timeout=30000)
        pg.wait_for_function('Number(document.querySelector("#lcl").dataset.frames)>0')
        pg.wait_for_timeout(1500)
        dims = pg.evaluate('() => [document.querySelector("#lcl").width, document.querySelector("#lcl").height]')
        W, H = dims
        getpx = '''(r) => Array.from(document.querySelector('#lcl').getContext('2d')
                     .getImageData(r[0],r[1],r[2],r[3]).data)'''
        # Toolbar rows: rows in the top band with many colour transitions.
        band = pg.evaluate('''([w,hi]) => { const g=document.querySelector('#lcl').getContext('2d');
          const out=[]; for (let y=0;y<hi;y+=2){ const d=g.getImageData(0,y,w,1).data; let t=0;
            for (let x=4;x<d.length;x+=4) if (Math.abs(d[x]-d[x-4])>30) t++; out.push([y,t]); }
          return out; }''', [W, 150])
        rows = [y for y, t in band if t > 8]
        print('toolbar candidate rows:', rows[:12])
        if not rows:
            sys.exit('no toolbar rows detected')
        ty = rows[len(rows)//2] if len(rows) > 2 else rows[0]
        # Paper box: widest white run on the vertical middle.
        paper = pg.evaluate('''(w) => { const c=document.querySelector('#lcl'); const g=c.getContext('2d');
          const d=g.getImageData(0, Math.floor(c.height/2), w, 1).data; let best=[0,0],run=-1;
          for (let x=0;x<w;x++){ const i=x*4; const wh=d[i]>250&&d[i+1]>250&&d[i+2]>250;
            if (wh){ if(run<0) run=x; } else if(run>=0){ if(x-run>best[1]-best[0]) best=[run,x]; run=-1; } }
          if (run>=0 && w-run>best[1]-best[0]) best=[run,w];
          const cx=Math.floor((best[0]+best[1])/2); const col=g.getImageData(cx,0,1,c.height).data;
          let y=0; while(y<c.height && !(col[y*4]>250&&col[y*4+1]>250&&col[y*4+2]>250)) y++;
          const y0=y; while(y<c.height && col[y*4]>250&&col[y*4+1]>250&&col[y*4+2]>250) y++;
          return [best[0],y0,best[1]-best[0],y-y0]; }''', W)
        print('paper box', paper)
        px, py = paper[0] + paper[2]//2, paper[1] + paper[3]//2
        region = [max(0, px-150), max(0, py-120), 300, 240]
        hits = []
        for x in range(8, min(paper[0], 900), 26):
            clean = pg.evaluate(getpx, region)
            pg.mouse.click(x, ty)
            pg.wait_for_timeout(120)
            pg.mouse.move(px-60, py-40); pg.mouse.down()
            for i in range(8):
                pg.mouse.move(px-60+i*16, py-40+i*10); pg.wait_for_timeout(12)
            pg.mouse.up(); pg.wait_for_timeout(200)
            after = pg.evaluate(getpx, region)
            d1 = diff(clean, after)
            pg.keyboard.press('Control+z'); pg.wait_for_timeout(250)
            undone = pg.evaluate(getpx, region)
            d2 = diff(clean, undone)
            if d1 > 500:
                hits.append((x, d1, d2))
                print(f'tool x={x:4} row={ty} painted={d1:6} afterUndo={d2:6} '
                      f'{"CREATE+UNDO" if d2 < 200 else "CREATE, undo incomplete"}')
        print('candidate tool buttons:', len(hits))
        b.close()
finally:
    srv.terminate()
