# TpX candidate for manual playtest

This archive is a build candidate. Read `BUILD.json` for the proposed version,
source commit, platform, and completed checks. Retain its adjacent `.sha256`
file when sharing the archive. Manual playtesting is required before release.

Extract the complete archive. Packaged preambles and help are read-only defaults;
TpX creates writable template copies in the user configuration directory when
needed. Preferences, recovery drafts and preview caches stay in the user
profile, so the application can run from a read-only install folder. The form
and icon resources are embedded in the executable. On macOS, the executable is
in `TpX.app/Contents/MacOS` and bundled preambles and help are in
`Contents/Resources`; keep the app bundle intact. `source.zip` contains the
exact project source.

For portable mode, pass `--portable` or place `TpX.portable` beside the
executable. Data goes into `TpX-portable-data/config`, `state` and `cache`.
On macOS, place the sentinel beside `TpX.app`; portable data stays beside the
bundle and does not change its sealed resources. **Help → Storage locations**
shows the active paths.

- Windows: run `TpX.exe`. This build targets 64-bit Windows with the native
  win32 backend; it does not require a Lazarus installation.
- Linux: run `./TpX` in an X11 or XWayland session. GTK2 runtime libraries are
  required (on Ubuntu, install `libgtk2.0-0t64`). The binary targets Ubuntu 24.04
  and newer compatible systems.
- macOS: open `TpX.app` in Finder, or run `open TpX.app` from Terminal, in a
  logged-in graphical session on Apple Silicon. This candidate uses Cocoa.
  It is ad hoc signed and unnotarized; see `INSTALL.md` for first-launch
  instructions.

The Mac bundle declares native `.tpx` drawings and includes the TpX document
icon. Linux packages include `install-linux-desktop-entry.sh` for optional
per-user MIME and launcher registration. Windows Open With instructions are in
`INSTALL.md`; no association is written when a package is extracted.

Install TeX and the desired external converters separately; see `README.md`
for prerequisites. Record operating system, candidate commit, display scaling,
and any failing steps when reporting a playtest result.
