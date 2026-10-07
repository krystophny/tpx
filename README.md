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

## Links
* http://tpx.sourceforge.net/
* https://sourceforge.net/projects/tpx/
* https://ctan.org/pkg/tpx?lang=de
