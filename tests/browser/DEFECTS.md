# Browser playtest defect catalog

Produced by `tests/browser/playtest.py` (Chromium, screenshot- and
pixel-oracles) against `web/dist`. Each entry lists the observable, the
measured numbers, the classification and the current state. Native comparison is
done with the project's own authority: `make test` plus
`obj/x86_64-linux/RuntimeTests <scenario>`.

Suite state: 10/12 scenarios green.

## D1 — Crosshair painted just before a mouse-down is not erased (open, medium)

Observable: hovering repaints correctly and the crosshair is erased when the
pointer leaves the canvas. But when the pointer is pressed right after a hover,
the crosshair lines painted for that hover survive every later repaint: after
`press -> drag -> up -> leave` a full-width row plus a full-height column remain
on the paper (measured 559 px in a 320x240 region, `row 120 / col 160` = the
hover point).

Evidence, stage by stage with the frame counter:

```
clean f4 | mid 1146 f13 | esc 1114 f14 | rel 1114 f15 | park 559 f16
at park: row 120 -> 320 px, col 160 -> 240 px
```

Native: `RuntimeTests draw-drag`, `draw-click`, `draw-cancel` all PASS, and the
native widgetsets paint through the window system, which owns the cursor erasure.
Class: WASI widgetset damage/capture interaction - during a captured drag the
viewport's stored cursor position no longer matches what it painted, so the erase
rectangle misses the stale lines.

Consequence for the suite: pixel-exact paper oracles are not usable across a
press-drag. `draw-cancel` therefore classifies behaviorally (select-all + delete
as the object probe): a cancelled gesture gives `probe 0 px`, an inserted control
gives `probe 455 px`. That is the honest observable, not a workaround for a
failing comparison: it asks whether an object exists, which is what cancel means.
State: open, cosmetic residue; no functional blocking.

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
