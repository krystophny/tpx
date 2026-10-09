# Browser playtest defect catalog

Baseline: TpX `efa73d6`, Lazarus fork `51f1b820df`, 2026-10-09.
All 19 browser scenarios passed in Chromium CI and locally in Chromium, Firefox
and WebKit. Scenarios inspect rendered pixels, saved drawing data, dialogs and
application state. Successful runs capture screenshots only with `--screenshots`;
failures retain a screenshot.

## Resolved

- **D2, excessive pointer CPU:** renderer-process CPU fell from 43 % to 6.66 %
  of one core during continuous 120 Hz pointer movement in the successful CI
  run. Precise damage, clipped paints, pointer coalescing and rendering only
  Pascal-invalidated frames replaced whole-form recomposition on every move.
- **D3, slow first frame:** CI recorded 770 ms, within the 2 s gate; the payload
  was 2.92 MB gzipped. Idle measurement recorded 0 frames over 12 s and 0.17 %
  of one core. These are measured baselines, not guarantees for every machine
  or document.
- **D4, message boxes terminated the instance:** synchronous browser dialogs
  now suspend and resume the correct Pascal call. The suite covers save
  confirmations, canceled dialogs, nested coordinate editing and repeated
  modal reopening without terminating the application.
- Crosshair motion and erasure, document keyboard focus, undo/redo, text input,
  resize and wheel zoom are covered by the current suite. Browser caret handling
  does not create a recurring idle repaint timer.
- Saving a drawing with a bitmap no longer launches `sam2p`: browser EPS data
  is generated from the decoded pixels. Native converter behavior is unchanged.

## Remaining limits

Full-document LaTeX compilation, MetaPost compilation and system printing need
external tools and remain desktop features. MetaPost source export is supported.
Open a drawing's referenced bitmap files through **Related files (optional)**;
the browser filesystem does not persist across reloads.

The browser font dialog accepts a family name but does not enumerate installed
fonts. The suite exercises core editing and file workflows; it does not establish
coverage of every TpX tool or LCL API.

## Verification commands

```bash
make web
python3 tests/browser/playtest.py
python3 tests/browser/playtest.py --browser firefox
python3 tests/browser/playtest.py --browser webkit
python3 tests/browser/perf.py
python3 tests/browser/playtest.py --screenshots  # optional visual review
xvfb-run -a make test                           # native regression checks
```

CI evidence: [browser run 37909825292](https://github.com/krystophny/tpx/actions/runs/37909825292).
