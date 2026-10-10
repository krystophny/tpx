# Signed distribution readiness

This note records the issue #98 preflight on 2026-10-11. It describes the
published v1.6.0 packages and the prerequisites for a later signed release; it
does not claim that Developer ID signing, notarization, or Authenticode is
configured.

## Current package and trust status

The published portable archives are compact: 6,389,100 bytes for Apple Silicon
macOS, 11,319,274 bytes for Windows x86-64, and 11,702,402 bytes for Linux
x86-64. The release contains no installer.

The macOS app passes `codesign --verify --deep --strict`. `codesign --display
--verbose=2` reports `Signature=adhoc` and `TeamIdentifier=not set`; its
`BUILD.json` records `macos_notarized: false`. After changing a sealed TeX
resource in a copy of the downloaded app, `codesign` rejected the bundle. This
signature detects bundle changes but does not identify TpX's publisher or
provide Gatekeeper notarization. The current first-launch instructions are in
[INSTALL.md](../INSTALL.md).

The current published release therefore remains ad-hoc and unnotarized. When a
credentialed signing path is enabled, ad-hoc output must be labelled for
development use only and stable releases must require the signed path.

The Windows install instructions state that the release is not
Authenticode-signed. It therefore has no verified publisher identity. A future
signature can display the publisher, but SmartScreen may still warn while a
new file or publisher builds reputation; do not promise that signing removes
all Windows prompts. The Linux archive remains portable and has no platform
publisher signature. SHA-256 checks detect changed downloads but do not
authenticate their publisher.

The GitHub repository currently has no Actions secrets, variables, or
environments, and this Mac has no valid code-signing identity in its keychain.
The Windows certificate store was not inspected. No Developer ID-signed,
notarized, or Authenticode-signed candidate was produced or tested.

## Distribution decision

Keep the portable ZIP and tarball packages and do not add an installer for this
release. They are about 6–12 MB, already extract into a user-owned folder, and
need no system-wide registration. An installer would add separate install,
upgrade, permission, and uninstall behavior without a demonstrated user need
or a requirement for system-wide integration. Revisit this if user demand
changes; do not add silent file associations or an updater.

## Requirements before enabling signing

1. The maintainer must approve real signing credentials. Put them in a
   pre-created, protected GitHub Actions environment with required reviewers and
   an environment deployment policy restricted to the release branch. A
   manual trusted release job may use that environment; pull-request and
   candidate jobs must remain secret-free. An environment name in workflow
   YAML alone is not a protection gate.
2. Sign the exact successful candidate artifact from the release run; do not
   rebuild it in the signing job. Record the input artifact digest, source
   commit, workflow run and attempt, signing identity, and notarization result
   in the manifest or adjacent provenance. Record the final artifact digest in
   the generated checksum file. Fail closed on missing or expired credentials,
   identity mismatch, rejected notarization, failed timestamp, or any
   post-sign integrity check.
3. On macOS, use a Developer ID Application identity with the hardened runtime
   and secure timestamp. Sign the app, place it in an archive, and submit that
   archive with `xcrun notarytool`; Apple does not accept a bare `.app` for
   upload. Staple the returned ticket to the app with `xcrun stapler`, create
   the final ZIP, then validate with `codesign --verify --deep --strict`,
   `xcrun stapler validate`, and `spctl --assess --type execute`. Apple describes
   notarization as an automated malware and code-signing check, not App Review;
   it does not promise a prompt-free launch. See Apple's [notarization
   guide](https://developer.apple.com/documentation/security/notarizing-macos-software-before-distribution)
   and [custom workflow](https://developer.apple.com/documentation/security/customizing-the-notarization-workflow).
4. On Windows, sign the executable with Authenticode using SHA-256 and an RFC
   3161 timestamp, then verify the signature and expected publisher with
   `signtool`. Microsoft documents the [`signtool` options](https://learn.microsoft.com/en-us/windows/win32/seccrypto/signtool)
   and explains [SmartScreen reputation](https://learn.microsoft.com/en-us/windows/apps/package-and-deploy/smartscreen-reputation).
5. Extend the `BUILD.json` and release-verifier contract additively. Keep
   ad-hoc, unnotarized candidates clearly marked as development builds; once
   signing is enabled, stable releases must carry the expected publisher and
   notarization status. The current `ci/verify_release.py` requires the
   macOS ad-hoc and unnotarized values. After signing, verify the extracted
   package and publisher identity, reject a deliberately tampered copy, and
   regenerate checksum files for the final artifacts before publication.

The real-credential candidate checks remain outstanding: macOS Developer ID
signature and notarization verification, Windows Authenticode publisher and
timestamp verification, expired or mismatched credential failure, secret
redaction, tamper rejection for both platforms, and portable install or move
checks on supported macOS and Windows versions. These checks cannot be claimed
from the current ad-hoc macOS signature or unsigned Windows package.
