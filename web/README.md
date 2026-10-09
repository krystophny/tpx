# TpX in the browser

TpX compiled to `wasm32-wasip1` against the experimental CustomDrawn/WASM
widgetset, with a small JavaScript host. The desktop build stays untouched:
`make` continues to use `lazbuild` and the native widgetset, `make web` builds
the browser target with the FPC cross compiler.

## Build

```sh
make web-check   # report whether the toolchain is present
make web         # obj/wasm32-wasip1/TpXWasm -> web/dist/tpx.wasm + host
make web-serve   # http://127.0.0.1:8791/
```

`web/build.sh` derives `web/build/TpXWasm.lpr` from the desktop `TpX.lpr`, so
the browser program always compiles the same unit list. Form resources are
produced by `lazres`, byte-identical to the `.lrs` files `lazbuild` produces for
the desktop build.

## Toolchain

`web/pins.env` holds the pinned revisions and the default checkout locations; every
entry is overridable (`WASM_LCL=/path/to/lazarus make web`). `ci/web-toolchain.sh`
recreates the layout from the pins on a clean machine.

| Component | Source | Role |
| --- | --- | --- |
| Lazarus | `krystophny/Lazarus` `feature/customdrawn-wasm-first-app` | CustomDrawn WASI widgetset, JOB DOM controls |
| FPC | `freepascal.org/fpc/source` main, see pin | `ppcrosswasm32`, wasm32-wasip1 RTL and fcl packages |
| pas2js | `krystophny/pas2js` `fix/job-packed-strings` | JOB object-release and packed-string repairs |

## Layout

| Path | Contents |
| --- | --- |
| `web/build.sh` | Compile wasm, transpile and bundle the host |
| `web/pins.env` | Pinned revisions and toolchain locations |
| `web/index.html`, `web/host.js` | Page shell, WASI/Canvas host loop |
| `web/jobhost.lpr` | pas2js JOB bridge (DOM controls ↔ widgetset) |
| `web/smoke.py` | Headless Chromium load check with a screenshot |
| `web/dist/` | Served artifacts (generated) |
| `web/build/` | Units, generated `.lpr`, `.lrs`, logs (generated) |

## Scope in the browser

Working: the drawing canvas, editing interactions, toolbars and dialogs, viewport
and export text generation, in-memory filesystem through the WASI shim.

The browser cannot run external programs: full-document LaTeX compilation,
MetaPost compilation and printing require desktop tools. Exporting MetaPost
source still works. Unsupported external-process actions show an error dialog
and leave the application interactive. Bitmap EPS data is generated directly
from decoded pixels in the browser; saving a drawing with a bitmap does not
require `sam2p`.

Open `.tpx` files with their referenced images using **Related files (optional)**
in the file dialog. The browser filesystem is temporary; keep the downloaded
`.tpx` and its image files together. Browser support is exercised through the
application scenarios below, not every LCL API or TpX drawing tool.

## Measured performance

`python3 tests/browser/perf.py` measures Chromium renderer-process CPU. The
successful CI run for `efa73d6` (2026-10-09) recorded:

| gate | measured | target | status |
|---|---|---|---|
| idle frames over 12 s | 0 | ~0 | pass |
| idle CPU | 0.17 % of a core | ~1 % | pass |
| CPU during continuous 120 Hz pointer move | 6.66 % of a core | < 10 % | pass |
| first rendered frame | 770 ms | < 2 s | pass |
| `tpx.wasm` gzipped | 2.92 MB | < 10 MB | pass |

Precise damage tracking, clipped control paints and coalesced pointer delivery
avoid whole-form repaint work. The host renders only when Pascal invalidates
pixels, so a stationary drawing does not run an animation loop. Measurements
vary by machine and drawing complexity; rerun the performance gates after
renderer changes.

## CI

`.github/workflows/web.yml` builds the toolchain from `web/pins.env`
(via `ci/web-toolchain.sh`, cached on the pins), runs `make web`, the bounded
headless Chromium suite and the performance gates. The pinned toolchain build
and all 19 scenarios passed in
[run 37909825292](https://github.com/krystophny/tpx/actions/runs/37909825292).
The same 19 scenarios also passed locally in Firefox and WebKit.

Normal successful runs do not capture screenshots. Failure screenshots are
retained; use `python3 tests/browser/playtest.py --screenshots` for a repeatable
visual review outside CI. See `tests/browser/DEFECTS.md` for the remaining scope
limits and verification commands.
