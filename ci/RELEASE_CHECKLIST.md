# TpX release process

`master` is the active feature-integration branch. There is no 1.7 feature line.
The optional `release/1.6` branch starts at `v1.6.0`. Cherry-pick reviewed
bug fixes and release-policy updates onto it; do not merge `master`, which
would import 2.0 feature work. Create
`release/2.0` from a reviewed `master` commit when 2.0 enters stabilization.
Its first release must be `v2.0.0-beta.1`; beta and RC numbers advance in order,
an RC requires the latest beta to be published, and `v2.0.0` stable requires a
published latest RC. Set `About_Version` in `src/tpx.inc` to the exact version
being packaged, including any beta or RC suffix.

## Candidate builds

Candidate builds have read-only repository permissions and only upload tested
artifacts; they never create tags or releases.

The desktop build workflow runs automatically for every push to `master` and
for pull requests. With no version input it uses `About_Version` from the
checked-out source; an explicit version must match that source. The README
links to the `master` push runs for downloading the latest fixes. Sign in to
GitHub, open a successful run and select the platform under **Artifacts**;
extract the artifact ZIP and then the package inside. Artifacts expire after
30 days. Each archive name and `BUILD.json` identifies its source commit.
These CI downloads require no release or tag.

To build a candidate from a release branch manually:

```sh
gh workflow run candidate.yml --ref release/1.6 -f version=1.6.1
gh workflow run candidate.yml --ref release/2.0 -f version=2.0.0-beta.1
gh run list --workflow candidate.yml
gh run download <run-id> --dir candidate-artifacts
```

Each release candidate runs the native core, GUI and export gates on Windows,
Linux and macOS. Linux also compiles actual TeX exports; macOS checks Cocoa
font/text behavior and its ad hoc app signature. Keep the source commit,
archive checksum, operating system and display scaling with manual playtest
results. Check drawing primitives, properties, colors, text, focus, shortcuts,
file handling, undo/redo and exports. Unsaved exit must honor Save, Discard,
Cancel and failed-save retry with one prompt per attempt. Preserve user
documents and open apps during tests, and report backend-specific coverage and
skipped scenarios.

## Publication

The publisher accepts only `release/1.6` with stable `1.6.x` patch versions and
`release/2.0` with `2.0.0-beta.N`, `2.0.0-rc.N` or `2.0.0`. It checks the
version against `src/tpx.inc`, rejects existing tags, and requires 1.6 sources
to descend from `v1.6.0`. Dispatch only after the exact source commit has been
reviewed and tested:

```sh
gh workflow run release.yml --ref release/1.6 -f version=1.6.1
gh workflow run release.yml --ref release/2.0 -f version=2.0.0-rc.1
```

The release job creates a tag only after all three platform packages pass the
required tests and artifact checks. It creates the tag at the approved source
SHA and refuses an existing tag. Windows x86-64, Linux x86-64 and Apple
Silicon macOS packages must carry that SHA, the exact source archive, required
test results, toolchain pins and checksums. The publisher rechecks all package
hashes, filenames, binary/help version metadata and platform manifests before
uploading. Only the final publish job has `contents: write`; it first creates a
draft and checks that every expected asset uploaded.

Release notes retain the reviewed `ci/RELEASE_NOTES.md` text, followed by
GitHub-generated notes from tagged history. Beta and RC notes start with an
explicit pre-release notice; each prerelease is marked as a pre-release and
Latest is explicitly disabled both when creating its draft and when publishing
it. Stable releases explicitly set Latest from the highest published stable
version, so a later 1.6.x hotfix cannot displace a published 2.0.0 release.
Package names retain the full beta/RC suffix. The macOS bundle version remains
the numeric `2.0.0` value required by macOS, while the executable, help
metadata, manifest and archive name retain the complete prerelease version.

If a run fails before tag creation, fix the cause and rerun the same version.
After tag creation, never move or delete the tag; it reserves that version. If
the draft cannot be resumed, use the next beta, RC or patch version. Leave any
failed draft unpublished while investigating.
If a published release has a defect, withdraw it using the repository's
available release controls and issue a corrected, higher patch or prerelease
version from the same supported branch. Do not edit published assets or
retarget the tag.

Support for 1.6.x ends after the 2.0.0 stable transition has passed the three
platform package gates and manual acceptance. Until then, 1.6.x receives only
reviewed hotfixes. No release is authorized by this checklist; publication
requires explicit authorization for that release.

Packages contain `INSTALL.md`, `README.md`, offline help, preambles, source,
license notices and `BUILD.json`. End-user instructions describe supported
architectures, portable installation, GTK2/X11 requirements, optional TeX
tools and Mac first launch. The macOS package remains ad hoc signed and
unnotarized; Developer ID signing and notarization are deferred.

## Toolchains

Windows uses the official Lazarus 4.8/FPC 3.2.2 installer with its published
SHA-256. Ubuntu 24.04 builds pinned Lazarus 4.8 with distribution FPC. macOS 15
builds pinned FPC 3.2.3 and Lazarus 4.8 to avoid the older compiler's Cocoa
linker problem. Unix builds apply the [versioned provider repairs](patches/README.md).
Exact revisions, patch hashes and build checks appear in each package's
`BUILD.json`.
