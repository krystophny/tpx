# TpX: drawing tool for TeX

This is the continuation of the TpX project that has been ported 
to the cross-platform Lazarus environment for Windows/Mac/Linux.

TpX is a lightweight, easy-to-use graphical editor for creating drawings and including them 
into LaTeX files in publication-ready form. It can also be used as a stand-alone editor for 
vector graphics. Up to version 1.5 it has been developed by Alexander Tsyplakov.

## Download and install

[Download TpX 1.6.0](https://github.com/krystophny/tpx/releases/tag/v1.6.0)
for Windows x86-64, Linux x86-64 or Apple Silicon macOS. Extract the archive and
run the editor; no compiler is needed. See [INSTALL.md](INSTALL.md) for the
short installation steps, dependencies and Mac first-launch instructions.
The Mac app is ad hoc signed and not notarized.

[![Linux tests](https://github.com/krystophny/tpx/actions/workflows/test.yml/badge.svg)](https://github.com/krystophny/tpx/actions/workflows/test.yml)

This is a screenshot of the v1.6.alpha.1 on Linux/GTK2
![Screenshot](/doc/screenshot_v1.6alpha1.png?raw=true "Screenshot")

## Build and run on Linux

TpX uses Free Pascal, Lazarus, the Lazarus Components Library (LCL), and the
`Printer4Lazarus` package included with Lazarus. GTK2 is the default Linux
backend. Building requires GNU Make and the GTK2 development libraries.

On Debian or Ubuntu:

```sh
sudo apt update
sudo apt install git make lazarus lcl-gtk2 fp-compiler fp-units-gfx libgtk2.0-dev
```

On Arch Linux or CachyOS:

```sh
sudo pacman -S --needed git make fpc lazarus gtk2
```

Clone, build, and start the editor:

```sh
git clone https://github.com/krystophny/tpx.git
cd tpx
make
make run
```

`make build` also builds the editor. The executable is
`obj/<cpu>-<os>/TpX`, for example `obj/x86_64-linux/TpX`.
An X11 display or XWayland is required to run the GTK2 editor.
The build stores its Lazarus configuration in the ignored `.lazarus/` directory.

The Makefile detects Lazarus in `/usr/lib/lazarus` or `/usr/share/lazarus`,
including versioned subdirectories used by Debian and Ubuntu.
For another installation location, pass the directory containing `lcl/`:

```sh
make LAZARUS_DIR=/path/to/lazarus
```

To build directly with Lazarus, use `lazbuild --ws=gtk2 TpX.lpi`.
If it reports that the Lazarus directory is invalid, add
`--lazarusdir=/path/to/lazarus`. Open `TpX.lpi` in the Lazarus IDE to develop
or debug the editor. Additional build flags can be passed through
`LAZBUILD_FLAGS`, for example `make LAZBUILD_FLAGS=--build-all`.

With Python 3 installed, run `make test` to check saved-drawing round trips
and SVG geometry. On a machine without a display, install Xvfb and use
`xvfb-run -a make test`. These checks also run in GitHub Actions on Linux.

### TeX and import/export tools

The editor can build and run without TeX. Previewing drawings and exporting
through LaTeX require `latex`, `pdflatex`, `dvips`, and the drawing packages.
Ghostscript (`gs`) supports PostScript conversion and previews, `mpost` supports
MetaPost output, and `pstoedit` supports PostScript/PDF import.

On Debian or Ubuntu:

```sh
sudo apt install texlive-latex-base texlive-latex-recommended texlive-latex-extra \
  texlive-pictures texlive-pstricks texlive-metapost ghostscript pstoedit
```

On Arch Linux or CachyOS:

```sh
sudo pacman -S --needed texlive-latex texlive-latexrecommended texlive-latexextra \
  texlive-pictures texlive-pstricks texlive-metapost ghostscript pstoedit
```

Bitmap-to-EPS conversion uses `sam2p` by default. Install it from your
distribution when available, or follow the [sam2p build instructions](https://github.com/pts/sam2p).
Its PNG, JPEG, and TIFF readers use Netpbm and libjpeg tools. Those are provided
by `netpbm` and `libjpeg-turbo-progs` on Debian/Ubuntu, or `netpbm` and
`libjpeg-turbo` on Arch/CachyOS. In TpX's settings, check the tool paths and
choose the viewers used to open previews.

### Drawing and TikZ defaults

Sized shapes support both dragging and click-to-click placement. In the Edit
menu, **Snap to shapes** attracts points to visible endpoints and corners
within six screen pixels. Shape snapping takes priority over grid snapping;
points farther away still use the grid when it is enabled.

Text can be selected across its visible area. Text properties offer left,
center, and right alignment. Enter a complete math expression such as
`$(a+b)$` in the text field, or use the TeX text field for other LaTeX code.
Desktop live previews typeset TeX text in the canvas using LaTeX and `dvipng`,
with reusable SVG output when `dvisvgm` is available. **View → Live LaTeX
Preview** toggles this behavior; it is enabled by default. Without the tools,
the canvas shows editable text. Explicit LaTeX previews and exports still work
as before.

On native desktop builds, **File → Automatic refresh → Auto refresh** is on by
default for source formats TpX can safely edit and save back. Pause refresh for
the current drawing or choose **Reload from disk…** to reload explicitly. If
external changes arrive while the drawing has unsaved edits, TpX keeps the
local scene and offers **Keep local edits** or **Save local copy…**; malformed
external files leave the current scene unchanged. Imported or refreshed TeX
content stays inert until **View → Trust TeX preview for this document** is
chosen. Browser builds do not watch local files; reopen or import the source to
refresh it.

TikZ output supplies drawing defaults for `\tpxLineWidth`, `\tpxTextSize`,
`\tpxDashSize`, and `\tpxDotSize`. Define these before including the drawing
to override them, for example:

```tex
\newcommand{\tpxLineWidth}{0.4mm}
\newcommand{\tpxTextSize}{10pt}
\input{drawing.TpX}
```

Object-specific widths and text heights remain proportional to these defaults.
Unset defaults belong to the drawing's local group and do not affect later
figures. Disable `FontSizeInTeX` to inherit the document's font size instead.

### Cropped LaTeX export

Export **PdfLaTeX source** and run `pdflatex` on the generated `.tex` file to
produce a PDF cropped to the drawing, including its configured border. This
uses the `preview` package supplied by the TeX prerequisites above. Exported
assets omit figure placement, centering, captions, and labels; those settings
remain in the editable drawing. Interactive document previews keep their
existing page layout.

**PDF from LaTeX EPS** also produces a cropped PDF and supports math text.

## Build and run on macOS

The Cocoa build was tested on Apple Silicon with Lazarus 4.8 and Free Pascal
3.2.3 from the upstream `fixes_3_2` branch. The older Homebrew FPC 3.2.2
compiler can fail with current Xcode linkers on Objective-C method atoms.
Install Xcode command-line tools and the bootstrap compiler:

```sh
xcode-select --install
brew install fpc git make python texlive ghostscript pstoedit
```

Build the maintained compiler in a private prefix:

```sh
git clone --depth 1 --branch fixes_3_2 https://gitlab.com/freepascal.org/fpc/source.git ~/code/fpc-source
cd ~/code/fpc-source
tpx_sdk=$(xcrun --sdk macosx --show-sdk-path)
make -j8 all FPC=/opt/homebrew/bin/fpc OPT="-XR$tpx_sdk" FPMAKE_BUILD_OPT="-XR$tpx_sdk"
make install INSTALL_PREFIX="$HOME/.local/tpx-fpc"
"$HOME/.local/tpx-fpc/bin/fpcmkcfg" \
  -d basepath="$HOME/.local/tpx-fpc/lib/fpc/3.2.3" \
  -d sharepath="$HOME/.local/tpx-fpc/share/fpc/3.2.3" \
  -o "$HOME/.local/tpx-fpc/etc/fpc.cfg"
ln -s ../lib/fpc/3.2.3/ppca64 "$HOME/.local/tpx-fpc/bin/ppca64"
export PATH="$HOME/.local/tpx-fpc/bin:/opt/homebrew/bin:$PATH"
export PPC_CONFIG_PATH="$HOME/.local/tpx-fpc/etc"
```

Then build Lazarus and TpX using that compiler:

```sh
git clone --depth 1 --branch lazarus_4_8 https://gitlab.com/freepascal.org/lazarus/lazarus.git ~/code/lazarus
cd ~/code/lazarus
# Apply the provider repairs used by the published binaries.
git apply ~/code/tpx/ci/patches/lazarus-4.8-opendocument.patch
git apply ~/code/tpx/ci/patches/lazarus-4.8-cocoa-font-cancel.patch
git apply ~/code/tpx/ci/patches/lazarus-4.8-cocoa-text-shortcuts.patch
make lazbuild LCL_PLATFORM=cocoa CPU_TARGET=aarch64 FPC="$HOME/.local/tpx-fpc/bin/fpc"
cd ~/code/tpx
make test WIDGETSET=cocoa LAZBUILD="$HOME/code/lazarus/lazbuild" \
  LAZARUS_DIR="$HOME/code/lazarus" LAZARUS_CONFIG="$PWD/.lazarus" \
  LAZBUILD_FLAGS="--compiler=$HOME/.local/tpx-fpc/lib/fpc/3.2.3/ppca64"
./obj/aarch64-darwin/TpX
```

The native GUI tests require a logged-in graphical session. Tool paths in
TpX's settings use Unix executable names on macOS, such as `pdflatex` and `gs`.

## Links
* http://tpx.sourceforge.net/
* https://sourceforge.net/projects/tpx/
* https://ctan.org/pkg/tpx?lang=de
