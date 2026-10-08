# TpX 1.6.0 release preparation

The proposed next minor release after `v1.6.alpha.1` is `v1.6.0`.
The candidate workflow uploads build artifacts only. It has read-only repository
permissions and never creates tags or GitHub releases. Publication stays on hold
until Chris completes manual playtesting and explicitly authorizes a release.

## Build a candidate

Integrate the reviewed product fixes and this workflow into the chosen branch.
The About dialog identifies this build as `Version 1.6.0 candidate`.
Verify its version before preparing another candidate.
Push the `fix/playtest-regressions` review branch or open a pull request to build
candidates automatically with proposed version `1.6.0`. After the workflow
exists on the default branch, manually dispatch **Candidate binaries (manual
playtest)** to select another reviewed branch or proposed version. The CLI
equivalent is:

```sh
gh workflow run candidate.yml --ref <reviewed-branch> -f version=1.6.0
gh run list --workflow candidate.yml
gh run download <run-id> --dir candidate-artifacts
```

GitHub requires a `workflow_dispatch` workflow to exist on the default branch.
The requested ref selects the exact source for the candidate build. Download
the Windows artifact as soon as that job finishes to playtest in the Windows VM;
the matrix jobs run independently. Retain each archive, checksum, and run URL.

## Automated gates and toolchains

Each job builds the editor and native GUI scenario runner, checks exported
geometry and saved-drawing behavior, then extracts its archive into a folder
containing spaces and repeats export checks against the shipped executable.
On macOS, this executable is inside the extracted `TpX.app` bundle; its TeX
preambles remain beside it in `Contents/MacOS`.
Linux also compiles TeX exports, both before and after packaging. Windows and
macOS leave TeX compilation to Linux; installed external tools require manual
checks on those systems. Failed gates prevent that platform's artifact upload.

Windows Server 2025 uses the official Lazarus 4.8/FPC 3.2.2 installer with its
published SHA-256. Ubuntu 24.04 builds the pinned Lazarus 4.8 source with the
distribution FPC. macOS 15 on Apple Silicon builds pinned `fixes_3_2` FPC 3.2.3
and Lazarus 4.8 sources to avoid the older compiler's Cocoa linker problem.
Unix builds apply the [versioned dependency repairs](patches/README.md); the
macOS candidate also runs native Cocoa font-dialog and text-shortcut tests.
All candidates include `BUILD.json`, project source, license notices,
`README.md`, and the TeX preambles. Form and icon resources are embedded.

Runner labels and action versions were checked against
[GitHub runner documentation](https://docs.github.com/en/actions/reference/runners/github-hosted-runners),
[checkout v6](https://github.com/actions/checkout/tree/v6), and
[upload-artifact v6](https://github.com/actions/upload-artifact/tree/v6).
The [Lazarus checksum page](https://www.lazarus-ide.org/index.php?page=checksums)
supplies the Windows installer hash. The setup-lazarus action was not used
because its version table ends at 4.4 and its maintainer announced discontinuation.

## Manual playtest before publication

- Record candidate commit, archive checksum, OS, architecture, and display scaling.
- Start the extracted editor in a folder with spaces; check About, menus,
  toolbar visibility, drawing, selection, dragging, zoom, and resizing.
- Save and reopen a drawing with shapes, text, math, custom colors, and fill.
  Check undo/redo, properties updates, selection appearance, and transient overlays.
- Preview and export with the installed TeX tools and chosen viewers. Confirm
  paths with spaces work, failures are shown, previews crop correctly, and exported
  files contain the intended colors, geometry, and math.
- Exercise unsaved exit with Save, Discard, Cancel, and a failed save; confirm
  each follows the chosen action and saved drawings reopen correctly.
- Review every reported regression against the original playtest steps on all
  three platforms. Attach failures to the exact candidate commit and rebuild
  after repairs; candidates from different commits do not share sign-off.
- Confirm Windows native behavior in the VM and macOS Cocoa behavior in a
  graphical session. Open the macOS `TpX.app` through Finder and confirm that
  it becomes the active application, with its native menu and Save As dialog.
  Obtain Chris's explicit publication authorization only
  after the remaining failures are resolved.

After authorization, a separate release operation can tag the approved commit
and publish its exact tested artifacts and release notes. This checklist and
workflow perform no such operation.
