# Source editing and external changes

TpX keeps a source path and format with a loaded drawing when the codec can
represent the document for save-back. Native TpX is editable. SVG and TikZ/
TeX save-back is limited to the documented profiles: [SVG source support](../docs/svg-document-support.md)
and [TikZ source support](tikz-support.md). SVGZ and documents with unsupported
source semantics are import-only; TpX reports why they cannot be saved back.
The importer does not fetch linked SVG resources or execute TeX.

## Save, undo, and external changes

A successful same-format save publishes a validated candidate and advances the
accepted disk revision. It keeps undo history: undoing after a save changes the
local drawing and marks it dirty, while the saved source remains unchanged until
another save. A failed parse, serialization, flush, or publication leaves the
accepted drawing, source identity, and history available for retry. Save As
changes the bound source only after its transaction succeeds.

Desktop source refresh is driven by operating-system file notifications. TpX
then reads and validates a stable snapshot before replacing a clean drawing.
Malformed or incomplete external content leaves the last valid drawing in
place. If local edits exist, TpX reports a conflict and retains both the local
scene and external file. Keeping local edits does not grant permission to
silently overwrite a later disk revision; source publication still checks the
accepted revision. Refresh and retry work is scheduled after an event, not by
filesystem polling.

Notifications are hints, not transaction boundaries for arbitrary programs.
The document service checks the disk revision before publication and retains a
recoverable prior version, but a last-check followed by replace is not an
atomic compare-and-swap against an uncooperative writer. Local in-place writes,
atomic replacement, rename, deletion and recreation are covered by the native
backend tests. Network, cloud-synchronized, removable, and unusual filesystems
may deliver weaker notifications; use manual reload when watching is reported
unavailable or degraded. See the [macOS watcher notes](file-watch-macos.md)
for path and filesystem behavior on that backend.

The native AutoSave preference is separate from local recovery. AutoSave is
off by default; when enabled, it publishes only a document that remains
eligible and conflict-free. Recovery drafts are stored separately from the
source document. A successful AutoSave or manual save does not erase undo
history. Browser storage is temporary: the browser has no external file watcher
or durable source path, so use upload, edit, and download to move source files.

Imported TeX starts with preview trust disabled. The user can grant trust for
the current document from the View menu; replacing the document revokes that
grant. Parsing and source editing never run TeX. Trusted rendering is performed
only on authored fixtures in the acceptance suite.

## Reproducing the acceptance checks

Run the strict parser and state tests without a display:

```sh
make test-core
```

The platform watcher suites run against native filesystem notifications. Linux
uses `make test-watch`; macOS uses `python3 tests/test_filewatch_macos.py`.
Windows runs the native IOCP runner in the Windows file-watch workflow. Each
suite uses a separate helper process and synchronized file changes.

Run the real Linux LCL and TeX regressions with a display server:

```sh
xvfb-run -a make test-gui test-tex
xvfb-run -a make test-source-acceptance
```

`test-source-acceptance` exercises production source open, refresh, graphical
edit, same-format save, undo/redo, failure cleanup, and 100 native-precision
TikZ save/reload cycles. It compares independently authored scenes and renders
trusted TikZ with LaTeX and SVG with librsvg. The driver writes case counts and
results to `obj/source-acceptance/report.json`; on failure it retains that
case's temporary sources and render artifacts under `obj/source-acceptance/retained/`.

Build and run the headless browser scenarios with `make web` followed by:

```sh
python3 tests/browser/playtest.py
```

The browser suite includes SVG and TikZ upload/edit/download, reopens the
result, and compares rendered pixels. `make web-check` verifies that the
checked-out Lazarus CustomDrawn/WASM tree matches `WASM_LCL_REV` in
[`web/pins.env`](../web/pins.env); local forks must set both `WASM_LCL` and
`WASM_LCL_REV`.

CI currently uses Ubuntu 24.04 with distribution FPC, Lazarus 4.8 and GTK2;
Windows Server 2025 with FPC 3.2.2 and Lazarus 4.8; and macOS 15 with FPC 3.2.3
and Lazarus 4.8. Browser builds use the exact revisions in `web/pins.env` and
run Chromium. Release packaging and its toolchain details remain in the
[release checklist](../ci/RELEASE_CHECKLIST.md).
