# Browser playtest defect catalog

Produced by `tests/browser/playtest.py` (Chromium, screenshot- and
pixel-oracles) against `web/dist`. Each entry lists the observable, the
measured numbers, the classification and the current state. Native comparison is
done with the project's own authority: `make test` plus
`obj/x86_64-linux/RuntimeTests <scenario>`.

Suite state: 10/12 scenarios green.

## D2 — Pointer-move recompose costs 43 % of a core (open, performance)

Measured by `tests/browser/perf.py` (Chromium, renderer process CPU):

| gate | measured | limit | status |
|---|---|---|---|
| first rendered frame | 2.6 s | < 2 s | FAIL |
| idle frames over 12 s | 0 | ~0 | pass |
| idle CPU | 0.08 % of a core | ~1 % | pass |
| CPU during continuous 120 Hz pointer move | 43 % of a core | < 10 % | FAIL |
| payload `tpx.wasm` gzipped | 2.89 MB | < 10 MB | pass |

Cause: `RenderForm` recomposes the whole 1068x791 form for every frame, so a
crosshair move that damages ~10k pixels repaints 844k of them. Native widgetsets
receive `rcPaint` in `WM_PAINT` and paint only the damaged clip.

Attempted and reverted (fork `0a258670cb`): passing the damaged rect as
`struct.rcPaint` and skipping controls outside it. It broke the first frame -
a render scheduled before the form is sized consumed the only dirty mark, the
image stayed 0x0, nothing ever presented again and the whole suite timed out. A
guarded retry recovered that case but the suite stayed red and the CPU gain was
illusory anyway, because the recompose, not the upload, is the cost. Kept from
that attempt: no blanket `InvalidateRect(nil)` in the pointer-move path, so TpX's
own precise cursor rects survive.

Proper fix, still open: thread `rcPaint` through `RenderForm` /
`RenderChildWinControls` **and** make the CustomDrawn control paints honour the
clip, then re-measure with `python3 tests/browser/perf.py`.

## D3 — First frame takes 2.6 s (open, performance)

16.7 MB module (2.89 MB gzipped), compiled and instantiated before the first
present. Not yet investigated; candidates are a gzipped serving path in
`web/serve.py` and measuring compile versus instantiate separately.

## D4 — A message box terminates the WASI instance (open, high)

Observable: with a `MessageBoxError` call inserted at `TMainForm.FormShow`, the page
never becomes ready and the host reports

```
state: error
text : Application error: exit with exit code 0
```

so the app exits instead of showing a dialog. Before this campaign the CustomDrawn
WASM `MessageBox` simply raised `ENotSupported`, which had the same user-visible
effect: nothing was ever reported and the instance died.

Changed so far (verified compile and suite, not the end-to-end dialog):
the widgetset `MessageBox` no longer raises - it forwards text, caption and flags
to a new host `message` import and answers `mrOk`/`mrYes` so the caller's logic
keeps working; the host renders a dismissible banner over the canvas
(`#notice`, styled in `web/index.html`). `SysBasic.FileExec` now reports the
missing external tool by name instead of returning False silently, so LaTeX
preview, sam2p bitmap import, MetaPost and printing explain themselves rather
than looking like broken no-ops.

Still open: something on the dialog path calls `proc_exit` (the exit code 0 shows
up in the WASI trace at boot too), so the banner has not been seen from Pascal
yet. Next step: trace `Application.MessageBox` in the WASI backend for the exit
call and remove it, then add a `degrade-*` scenario per feature asserting the
banner text and that the app stays interactive afterwards.
Until then `graceful-degrade` cannot be claimed: the paths are silent-or-fatal
in practice, which the contract explicitly forbids.

## Fixed in this campaign


### Pointer moves and crosshair tracking (was: input stopped after the first frames)

`TViewport2D.MouseMove` paints its crosshair straight onto the control canvas.
On native widgetsets that writing reaches the window immediately; in the browser
the frame is composed from the control image, so a move that only painted never
dirtied the frame and the cursor never followed the pointer (measured 0 changed
pixels). Two widgetset fixes:

1. Invalidate the hovered `TCustomControl` on every pointer move (fork
   `9bcd19690c`), chrome controls excluded so hovering panels/toolbars stays free
   of repaints. Crosshair repaint went from 0 to 1114 changed pixels.
2. Deliver mouse enter/leave the way a window system does (fork, this commit):
   enter sets `FMouseInClient`, leave clears it and the application erases its
   cursor. `TControl.CMMouseLeave` fires its event only when `LParam = 0`, which
   is why the first attempt (passing the control) still left the cursor stuck.
   Measured: after the pointer leaves the canvas the paper returns to **0**
   changed pixels.

### Ctrl+Z / redo now work (was: object survived undo)


Symptom: `Ctrl+Z` left 559 px of a 1016 px drawing. The residual was a
full-width row plus full-height column — the viewport crosshair frozen at the
drag endpoint (D1) — not the object. With the document holding keyboard focus
and the frame repainting, undo removes the drawing down to 0 px and redo
restores it.

Two real causes were fixed:

1. WASI CustomDrawn `CallbackMouseDown` handed keyboard focus to toolbars and
   panels on every click, so Escape/Ctrl+Z/arrows were delivered to the chrome
   instead of the document. Focus now stays with the document, mirroring the
   native widgetsets (fork commit, `fix-widgetset`).
2. The suite itself measured with the pointer inside the compared region,
   counting the crosshair as leftover ink.

### Caret blink no longer burns the idle tab

`TCDEdit.DoEnter` started a 500 ms caret-blink timer; the browser backend
composes and uploads the whole window surface per invalidate, so an idle tab
moved ~3.4 MB twice a second forever. The caret is shown steadily in the
browser instead. Measured: **idle repaints 2 fps → 0 frames in 4 s**, renderer
CPU ~0.2 %. Native backends are untouched.

### Keyboard, wheel and window resize reach LCL

Keyboard/`lcl_key`, wheel/`lcl_wheel` and modifier state through `GetKeyState`
were missing entirely before; verified by `text-input` (705 px of typed text),
`wheel-zoom` (3630 px zoomed, restored to 619 px) and `resize-follows`
(canvas 1068 → 968).

## Verification commands

```bash
make web                                            # build wasm + host
python3 tests/browser/playtest.py                 # browser campaign + manifest
xvfb-run -a make test                             # native authority, unchanged
xvfb-run -a obj/x86_64-linux/RuntimeTests draw-cancel
```
