# TpX release process

Releases are built from a reviewed commit on `master`. Chris authorized
publication of 1.6.0 after testing the repaired Mac app. The macOS package uses
an ad hoc signature; Developer ID signing and notarization remain deferred.

## Candidate builds

The candidate workflow has read-only repository permissions and uploads
artifacts without creating tags or releases:

```sh
gh workflow run candidate.yml --ref master -f version=1.6.0
gh run list --workflow candidate.yml
gh run download <run-id> --dir candidate-artifacts
```

Record the source commit, archive checksum, operating system and display
scaling for manual playtesting. Check drawing primitives, properties, colors,
text, focus, shortcuts, file handling, undo/redo and exports. Unsaved exit must
honor Save, Discard, Cancel and failed-save retry with one prompt per attempt.
Preserve user documents and open apps during tests. Report backend-specific
coverage and skipped scenarios explicitly.

## Publication

After explicit user authorization, dispatch **Publish release** on `master`:

```sh
gh workflow run release.yml --ref master -f version=1.6.0
```

The workflow rejects other branches, mismatched About versions and existing
tags. Its reusable matrix builds Windows x86-64, Linux x86-64 and Apple Silicon
macOS packages from the dispatch commit. Each platform runs native export and
GUI regression gates, then repeats export checks on the extracted package in
a path containing spaces. Linux also compiles actual TeX exports; macOS runs
Cocoa font/text regressions and verifies the app's ad hoc code signature,
including rejection of a modified sealed resource. All gates must pass.

A separate publishing job checks archive hashes, exact source snapshots,
required runtime resources and build provenance. Only this job has
`contents: write`. It creates a draft with all assets, checks the asset list,
and publishes `v<version>` as the latest stable release. Failed draft uploads
remain unpublished and require investigation before retrying.

Packages contain `INSTALL.md`, `README.md`, offline help, preambles, source,
license notices and `BUILD.json`. The release also supplies a source ZIP and
`SHA256SUMS`. End-user instructions describe supported architectures, portable
installation, GTK2/X11 requirements, optional TeX tools and Mac first launch.
After publication, download all public assets and verify them against the CI
artifacts and tag commit.

## Toolchains

Windows uses the official Lazarus 4.8/FPC 3.2.2 installer with its published
SHA-256. Ubuntu 24.04 builds pinned Lazarus 4.8 with distribution FPC. macOS 15
builds pinned FPC 3.2.3 and Lazarus 4.8 to avoid the older compiler's Cocoa linker
problem. Unix builds apply the [versioned provider repairs](patches/README.md).
Exact revisions and patch hashes are recorded in each package's `BUILD.json`.
