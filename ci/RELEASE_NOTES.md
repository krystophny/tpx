TpX 1.6.0 brings the Lazarus drawing editor to Windows, Linux and Apple Silicon
macOS, with fixes from automated checks and native GUI playtesting.

Download the package for your platform, extract it, and run `TpX.exe`, `TpX.app`
or `./TpX`. See [installation instructions](https://github.com/krystophny/tpx/blob/v1.6.0/INSTALL.md)
for dependencies, checksum verification and optional TeX tools. No compiler is
needed to use these packages.

- Windows: portable 64-bit ZIP; tested on Windows Server 2025.
- macOS: Apple Silicon app ZIP; built on macOS 15, tested on macOS 26.
  The app is **ad hoc signed and not notarized**. This signature seals code and
  resources but is not an Apple Developer ID signature. First launch may require
  **System Settings → Privacy & Security → Open Anyway**.
- Linux: x86-64 tarball; Ubuntu 24.04-compatible libraries, GTK2 and X11/XWayland
  required. Extract into a writable folder in your home directory.

Drawing fixes cover circle and line widths, initial zoom, shape placement,
snapping, text hit testing, font sizes, object properties and custom colors.
Export fixes cover TikZ defaults, standalone LaTeX labels, cropped PDF output,
bitmap conversion and external tool paths containing spaces.

macOS now uses Command editing shortcuts, respects text-field editing, handles
font-dialog cancellation correctly, and stores preferences outside the signed
app. Closing an unsaved drawing follows Save, Discard or Cancel with one prompt;
failed saves leave the drawing available for retry.

Every package is built and tested in GitHub Actions. Checks cover saved drawing
behavior, geometry and exports, native GUI scenarios, and the extracted package.
Linux also compiles actual TeX exports; macOS checks Cocoa font/text behavior,
code-signature validity and detection of modified resources. GUI coverage varies
by backend; physical pointer scenarios that require GTK are tested on Linux.

Packages include source, license notices, preambles, offline help and a build
manifest. External TeX engines and converters are installed separately.
Live LaTeX rendering directly on the editing canvas remains future work (#19).

Chris&AI
