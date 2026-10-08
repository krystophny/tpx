# TpX candidate for manual playtest

This archive is a build candidate. Read `BUILD.json` for the proposed version,
source commit, platform, and completed checks. Retain its adjacent `.sha256`
file when sharing the archive. Manual playtesting is required before release.

Extract the complete archive into a writable folder. Keep `preview.tex.inc`
and `metapost.tex.inc` beside the executable; TpX reads these customizable
preambles during TeX previews and MetaPost exports. The form and icon resources
are embedded in the executable. On macOS, the executable and preambles are in
`TpX.app/Contents/MacOS`; keep the application bundle intact. `source.zip`
contains the exact project source.

- Windows: run `TpX.exe`. This build targets 64-bit Windows with the native
  win32 backend; it does not require a Lazarus installation.
- Linux: run `./TpX` in an X11 or XWayland session. GTK2 runtime libraries are
  required (on Ubuntu, install `libgtk2.0-0t64`). The binary targets Ubuntu 24.04
  and newer compatible systems.
- macOS: open `TpX.app` in Finder, or run `open TpX.app` from Terminal, in a
  logged-in graphical session on Apple Silicon. This candidate uses Cocoa.
  It is unsigned and unnotarized; follow your system's normal review process
  for downloaded software.

Install TeX and the desired external converters separately; see `README.md`
for prerequisites. Record operating system, candidate commit, display scaling,
and any failing steps when reporting a playtest result.
