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

Disabled on purpose, because WASI cannot run external programs: LaTeX preview,
`sam2p` bitmap conversion, MetaPost and printing. Those menu entries report that
they are unavailable in the browser instead of failing silently.
