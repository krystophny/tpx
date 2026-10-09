# Browser playtest defect catalog

Produced by `tests/browser/playtest.py` (Chromium, screenshot- and
pixel-oracles) against `web/dist`. Each entry lists the observable, the
measured numbers, the classification and the current state. Native comparison is
done with the project's own authority: `make test` plus
`obj/x86_64-linux/RuntimeTests <scenario>`.

Suite state: 10/12 scenarios green.

## D1 — Pointer input stops after a mouse press (blocker)

Observable: hovering repaints the canvas (crosshair follows, status-bar
readout updates: 375 px changed over 4 moves). After a `pointerdown`/`move`/
`pointerup` sequence the application stops reacting to the pointer completely:

```
after drag, moving crosshair diff: 0
hover frames 3  after drag 3  after move 3   # frame counter frozen
```

The frame counter stops advancing, so the render scheduler never runs again.
Native: `RuntimeTests draw-drag`, `draw-click`, `draw-cancel` all PASS, so the
TpX logic is fine.

Class: WASI widgetset / browser host JS (mouse capture and coalescing state).
State: open — highest priority, it gates every interactive scenario.

Repro: `python3 tests/browser/playtest.py --only keyboard-shortcut`
(the scenario's own oracle is unaffected; use `web/diag-input.py` for the
frame numbers) or the sequence click → press → move → up → move.

## D2 — Escape during a drag does not cancel (depends on D1)

`draw-cancel`: preview 1146 px, after Escape 1114 px remain. The difference
between preview and leftover is exactly the crosshair pair (width + height - 1
= 559 px at the drag endpoint), i.e. the preview object is covered by the
frozen crosshair measurement. Native `RuntimeTests draw-cancel` PASSes via
`Msg_Escape`.

Class: same as D1; the scenario must re-read pixels after the pointer has been
parked on the paper and the frame has actually been repainted.
State: open, expected to fall out of the D1 fix.

## Fixed in this campaign

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
