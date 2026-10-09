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
| pas2js | `krystophny/pas2js` `fix/job-release-object-id` | JOB object-release repair (upstream MR !103, unmerged) |

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

Disabled because WASI cannot run external programs: LaTeX preview, `sam2p`
bitmap conversion, MetaPost and printing. `SysBasic.FileExec` names the missing
tool in a report instead of returning failure silently, and the widgetset answers
a message box through a host banner rather than raising. **Not yet working end to
end**: with a probe dialog the instance exits with code 0 instead of showing the
banner, so today those paths are silent-or-fatal. Tracked as D4 in
`tests/browser/DEFECTS.md`, which is the place to read before trusting any of
this table.

## Measured performance

`python3 tests/browser/perf.py` (Chromium, renderer process CPU):

| gate | measured | target | |
|---|---|---|---|
| idle frames over 12 s | 0 | ~0 | pass |
| idle CPU | 0.08 % of a core | ~1 % | pass |
| CPU during continuous 120 Hz pointer move | 43 % of a core | < 10 % | **FAIL** |
| first rendered frame | 2.6 s | < 2 s | **FAIL** |
| `tpx.wasm` gzipped | 2.89 MB | < 10 MB | pass |

The pointer number is the whole-form recompose in `RenderForm`: a crosshair move
damages ~10k pixels and repaints 844k. A dirty-rect attempt was reverted
(widgetset `0a258670cb`) because it swallowed the first frame and the upload was
never the cost; the fix has to thread `rcPaint` through `RenderForm`
/`RenderChildWinControls` and honour the clip inside control paints.

## CI

`.github/workflows/web.yml` builds the toolchain from `web/pins.env`
(via `ci/web-toolchain.sh`, cached on the pins), runs `make web`, the bounded
headless Chromium suite and the performance gates, and uploads the screenshot
manifest. Desktop workflows (`test.yml`, `candidate.yml`) are untouched and
`Linux tests` passes on this branch.

Current CI state, honestly: the web job fails at **Build the pinned WASI
toolchain**. The pinned FPC source tree is flat (no `fpc/` subdirectory and no
`configure` in this checkout), so the cross-compiler stage in
`ci/web-toolchain.sh` has never been executed successfully anywhere - it was
written from the documented layout, and CI is the first place that found out.
Next step: reproduce the local toolchain build commands from
`~/code/fpc-lcl-wasm` (its RTL is already built for wasm32-wasip1) in the
script, or publish the toolchain as a release artifact and have CI restore it.
