# Test layers

`make test` is the aggregate target for the current suites. It runs the strict
core checks, the real application/export and LCL runtime tests, and the TeX
rendering tests. The Linux CI job continues to run it under Xvfb.

`make test-core` compiles `tests/core/CoreTests.lpr` with Free Pascal's debug,
range, overflow, I/O, stack, and assertion checks. It does not use Lazarus,
LCL, a window system, or TeX. The driver removes `DISPLAY` and
`WAYLAND_DISPLAY` and runs the binary with an empty `PATH`, a private working
directory, and a private home directory. It also checks that a wrong geometric
expectation and a failing scenario each produce a useful failure report and a
nonzero process exit. The machine-readable result is retained at
`obj/<cpu>-<os>/core-tests/core-report.json`.

The initial core cases exercise the production TpX comment-envelope extractor
and XML DOM codec against independently authored source fixtures. They check
document metadata, nested object order, line geometry and style attributes,
Unicode and TeX text attributes, image links, and the empty-document form.
These tests do not exercise the LCL scene loader or drawing dirty state; those
remain in the application runtime layer until their production seams are
available.

`tests/core/suite.json` records the expected suite and case counts. Every core
test unit registers its cases with `CoreTestSupport.RegisterCoreTest`; update
the manifest count when adding cases. A missing or zero-match suite fails.
`python3 tests/test_core.py --filter=<case-name-substring>` runs a filtered
subset while still checking the complete discovered count.

`make test-gui` builds TpX and runs the existing export and LCL runtime suites.
`make test-tex` also launches the LCL application for export. On Linux, run both
targets under `xvfb-run -a` as the CI workflow does. The TeX target requires
`pdflatex`, so the suite cannot pass with every case skipped. Existing
Ghostscript, dvips, and sam2p cases retain their explicit dependency-based
skips when those optional paths are unavailable.

`make test-watch` filters the core suite for watcher cases. It intentionally
fails while no watcher case is registered; the watcher backend issue adds the
first such cases. The aggregate target will include that layer when the
production watcher suite is introduced.
