# Install TpX 1.6.0

Download the package for your computer from the
[GitHub release](https://github.com/krystophny/tpx/releases/tag/v1.6.0).
The packages include the editor, TeX preambles, offline help, source and licenses.
You do not need Lazarus or Free Pascal to run them.

## Windows

Download `tpx-1.6.0-windows-x86_64.zip`. Extract the whole ZIP into a folder
owned by your user, such as `Documents\TpX`, then double-click `TpX.exe`.
You can create a desktop shortcut to it. This is a portable 64-bit application;
its preferences are saved beside the executable. Avoid `Program Files`.
The release is tested on Windows Server 2025 with Desktop Experience.
Windows may ask you to review this download because it is not Authenticode-signed.

## macOS

Download `tpx-1.6.0-macos-aarch64.zip` for an Apple Silicon Mac. Extract it and
move `TpX.app` into `~/Applications` (your home folder's Applications folder),
then double-click it. The release is built on macOS 15 and tested on macOS 26.
An Intel Mac needs a source build; this release does not include an Intel binary.

The app has a verified **ad hoc code signature**. This seals the app's code and
resources, but does not authenticate the publisher with an Apple Developer ID.
It is **not notarized**. If macOS blocks the first launch, attempt to open it,
then go to **System Settings → Privacy & Security → Open Anyway** and confirm
**Open**. This creates an exception for this app; later launches work normally.
See [Apple's instructions](https://support.apple.com/en-us/102445).
A managed Mac may prevent this exception.

Keep the app bundle intact. Preferences are saved in your user configuration
folder. Bundled defaults and help are sealed in `Contents/Resources`.
The template-editing menu opens writable copies in your configuration folder;
changes there preserve the app signature. Editing files inside the app
invalidates its signature.

## Linux

Download `tpx-1.6.0-linux-x86_64.tar.gz` for 64-bit x86 Linux. This binary is
built on Ubuntu 24.04 and needs compatible system libraries, GTK2, and an X11
or XWayland graphical session. On Ubuntu 24.04 install the GTK2 runtime:

```sh
sudo apt install libgtk2.0-0t64
```

Extract into your home directory and run:

```sh
tar -xzf tpx-1.6.0-linux-x86_64.tar.gz
cd tpx-1.6.0-linux-x86_64
./TpX
```

Keep the folder writable: preferences are saved beside the executable. Keep
`preview.tex.inc`, `metapost.tex.inc` and `help/` there too. Other distributions
may need a source build; see [README.md](README.md).

## TeX and converters

Drawing, saving, SVG export and writing TikZ source work without a TeX
installation. Typeset previews and LaTeX-derived PDF exports need TeX and the
corresponding drawing packages. Install TeX Live or MiKTeX on Windows, MacTeX
on macOS, or your distribution's TeX Live packages on Linux. Set executable
and viewer paths in TpX's settings if the tools are not on PATH.
Ghostscript, MetaPost, pstoedit and sam2p enable their respective conversion
features and are installed separately. See the tool list in [README.md](README.md).

## Verify a download

Each archive has an adjacent `.sha256` file, and `SHA256SUMS` lists all release
assets. Compare the checksum before extracting:

```sh
# Linux
sha256sum tpx-1.6.0-linux-x86_64.tar.gz
# macOS
shasum -a 256 tpx-1.6.0-macos-aarch64.zip
```

On Windows use PowerShell:

```powershell
Get-FileHash .\tpx-1.6.0-windows-x86_64.zip -Algorithm SHA256
```

`BUILD.json` records the exact source commit, toolchain, dependency patches,
completed checks and Mac signing status. The included `source.zip` is the
source snapshot for that executable.
