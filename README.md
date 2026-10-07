# TpX: drawing tool for TeX

This is the continuation of the TpX project that has been ported 
to the cross-platform Lazarus environment for Windows/Mac/Linux.

TpX is a lightweight, easy-to-use graphical editor for creating drawings and including them 
into LaTeX files in publication-ready form. It can also be used as a stand-alone editor for 
vector graphics. Up to version 1.5 it has been developed by Alexander Tsyplakov.

## Current status

* Linux: usable for testing with the GTK2 backend
* macos: unusable due to a bug with canvas drawing on both carbon and cocoa backend
* Windows: usable for testing with the win32 backend

[![Build Status](https://travis-ci.org/krystophny/tpx.svg?branch=master)](https://travis-ci.org/krystophny/tpx)

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
The canvas shows the editable text; LaTeX previews and exports typeset it.

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

## Links
* http://tpx.sourceforge.net/
* https://sourceforge.net/projects/tpx/
* https://ctan.org/pkg/tpx?lang=de
