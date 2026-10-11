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
entry is overridable. The build verifies the Lazarus fork's checked-out Git revision
against `WASM_LCL_REV` before compiling. To test a local fork, set both
`WASM_LCL=/path/to/lazarus` and `WASM_LCL_REV=<that checkout's commit>` for `make web`.
`ci/web-toolchain.sh` recreates the layout from the pins on a clean machine.

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
TeX text labels have cached MathJax previews; the typesetter loads only when
a drawing contains TeX text. Imported TeX labels remain inert until **View →
Trust TeX preview for this document** is selected. The approval applies only to
the current drawing; opening another source clears it. The View menu can
disable live previews.

The browser cannot run external programs: full-document LaTeX compilation,
MetaPost compilation and printing require desktop tools. Exporting MetaPost
source still works. Unsupported external-process actions show an error dialog
and leave the application interactive. Bitmap EPS data is generated directly
from decoded pixels in the browser; saving a drawing with a bitmap does not
require `sam2p`.

Open `.tpx` files with their referenced images using **Related files (optional)**
in the file dialog. The browser filesystem is temporary; keep the downloaded
`.tpx` and its image files together. Supported SVG and TikZ sources can be
opened, edited and saved in their source format. Every browser-opened file is a
snapshot without desktop filesystem access; the status notice explains that
external auto-refresh is unavailable and **File → Open** re-imports changes.
**File → Download a copy** requests a browser download of the current document
and linked assets; the status reports that request without claiming the browser
saved it to disk or synchronized the original file. A best-effort leave-page
warning stays active while the drawing has unsaved work and after a download
request whose completion cannot be confirmed. Starting a new drawing or opening
another one clears that request warning. Browsers may suppress the warning, so
keep an explicit downloaded copy.

## Inspector and text editing

**Edit → Object properties** opens the right-side Inspector; use **View → Object
inspector** to show or hide it, and drag its splitter to resize the panel. Its
sections can be collapsed to leave more room for the canvas. Mixed selections
show **Mixed** for fields whose values differ. Editing one field applies that
property to the selection while preserving each object's other properties.
Owner-drawn dropdowns in the browser open an in-page modal **Choose item**
dialog; ordinary numeric and text edits stay inline.

Create a label with the Text tool or **Insert → Insert text**, then type in the
canvas editor. Click an existing label with the Text tool or double-click it in
selection mode to edit it. The **Text** and **TeX source** buffers are
independent; Enter inserts a line break, while Ctrl+Enter (or Command+Enter on
macOS), Tab, or **Done** finishes a valid edit. Escape cancels it after active
input-method composition ends. Save actions settle a valid inline draft.
Coordinates, transforms, scale and preferences keep their
dedicated dialogs; file selection and explicit discard/replacement safeguards
also remain dialogs.

For an end-to-end check, draw a line and a rectangle, create and type a canvas
label, then use the Inspector to change line width, end arrow and text height.
Use Tab to commit a field, edit the same label with the Text tool and selection
mode's double-click, and check Undo/Redo. Collapse and resize the Inspector and
workspace, then save and reopen to confirm the geometry, style, Text and TeX
source remain. Where a live input method is available, press Escape during a
marked composition and verify cancellation waits until that composition ends.

Automated browser interaction scenarios use Playwright with desktop Chromium
viewports. They do not certify tablet or phone layouts, touch or Apple Pencil
input, or screen-reader operation. Native composition regressions use synthetic
platform composition hooks; they do not claim coverage of a live operating
system input method on physical hardware.

Browser support is exercised through the application scenarios below, not every
LCL API or TpX drawing tool.

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
