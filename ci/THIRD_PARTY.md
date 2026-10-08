# Component licenses and sources

TpX is distributed under the GNU General Public License version 2, included
as `LICENSE`. Its exact source snapshot is included as `source.zip`; the
snapshot also preserves the notices in vendored PowerPdf, XML, and other
components. PowerPdf's source headers specify the GNU Library General Public
License version 2 or later.

The executable links the Lazarus Components Library and Free Pascal runtime.
The Lazarus distribution's license texts are included in `licenses/`.
Lazarus documents the LCL linking exception in its modified LGPL text.
Compiler and Lazarus revisions are recorded in `BUILD.json`.
The Unix toolchain applies the versioned Lazarus patches under `ci/patches/`:
quoted-path handling in `OpenDocument`, native Cocoa font-picker cancellation,
and Cocoa text-shortcut responder handling. `BUILD.json` records the SHA256 of
each applied patch. The macOS candidate job runs the native Cocoa font-dialog
and text-shortcut regressions. Provider source bases, patch hashes, and current
owner-commit status are documented in `ci/patches/README.md`.

Upstream source repositories:

- [Lazarus](https://gitlab.com/freepascal.org/lazarus/lazarus)
- [Free Pascal](https://gitlab.com/freepascal.org/fpc/source)
- [TpX](https://github.com/krystophny/tpx)

TeX, Ghostscript, viewers, and import converters are optional external tools
installed separately; this archive does not bundle them.
