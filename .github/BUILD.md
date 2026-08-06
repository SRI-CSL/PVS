# GitHub Actions Build And macOS Packaging Notes

This directory contains the GitHub Actions workflows used to build PVS bundles for Apple Silicon, Apple x86, Linux ARM, and Linux x86.

The top-level release entry point is [.github/workflows/release-builds.yml](./workflows/release-builds.yml). It runs the four platform workflows as reusable builders, waits for them to finish, then performs GitHub Release publication and stale-asset cleanup in one final job. The per-platform workflows still support `workflow_dispatch` for standalone debugging, but they no longer publish directly to GitHub Releases on push.

This document focuses on the current packaging, notarization, and release flow, using Apple Silicon as the concrete example. The Apple x86 and Linux workflows mirror the same structure.

## Current Apple Silicon Flow

The Apple Silicon workflow is defined in [.github/workflows/apple-silicon-build.yml](./workflows/apple-silicon-build.yml). In normal release operation it is called from [.github/workflows/release-builds.yml](./workflows/release-builds.yml).

It has two jobs:

1. `build-macos-arm64`
   This job always runs. It builds the standalone Apple Silicon bundle, smoke-tests it, and uploads:
   - `pvs-<branch>-<date>-macos-arm64.tgz`

2. `package-macos-arm64`
   This job runs automatically whenever all required signing and notarization secrets are present. It downloads the standalone tarball from the first job, rebuilds a macOS installer package from it, signs the package, notarizes it, staples the notarization ticket, and uploads:
   - `pvs-<branch>-<date>-macos-arm64.pkg`

The split is intentional: the signed and notarized package path should not block the plain standalone tarball and bundle build.

Before building PVS itself, the Apple Silicon workflow installs Homebrew's native Apple Silicon `sbcl` package only as the bootstrap host, then builds a custom SBCL `2.6.3` install tree from source via [.github/scripts/install-patched-sbcl-source.sh](./scripts/install-patched-sbcl-source.sh). PVS is built against that custom SBCL, and the bundle step copies that active SBCL support tree into the release so the packaged runtime matches the source-built SBCL layout instead of shipping Homebrew's SBCL runtime directly.

Platform workflows prepare a neutral build tree under `/tmp` before configuring or compiling PVS. The original GitHub checkout lives under the runner home, for example `/Users/runner/work/...` on macOS and `/home/runner/work/...` on Linux; SBCL cores, SBCL saved shared-object metadata, and native debug sections can retain compile-time paths. Before saving the SBCL core, the build discards build-time shared objects so the image does not try to reopen `src/.../file_utils`, `src/.../mu`, or `src/.../ws1s` on an end-user machine; runtime startup loads the packaged copies from `PVS_RUNTIME_DIR`. Each tarball is also extracted and audited before upload by [.github/scripts/audit-release-artifact.sh](./scripts/audit-release-artifact.sh), which fails the job if the runner checkout, neutral build tree, runner temp directory, runner tool cache, or runner home path appears in the payload.

For the SBCL runtime, the packaged bundle now carries the generated `pvs-sbclisp` and `pvs-sbclisp-bin` launchers, the saved `pvs-sbclisp.core`, and a bundled `runtime/sbcl/` tree containing the SBCL executable plus its `lib/sbcl/` support files. The launchers set `SBCL_HOME` to that bundled tree at runtime instead of assuming only a copied bare executable is sufficient.

The packaged install base is `/PVS`. That means:

- installing with `-target /` places the bundle at `/PVS/pvs-8.1`
- installing with `-target CurrentUserHomeDirectory` places it at `~/PVS/pvs-8.1`

Release publication is controlled by [.github/release-config.env](./release-config.env):

- `PVS_RELEASE_STABLE_BRANCH`
- `PVS_RELEASE_DEV_BRANCH`
- `PVS_RELEASE_VERSION`

Those variables determine the stable branch used to validate official release
tags, the branch that feeds the rolling development prerelease, and the PVS
version embedded in release tags.

The current release policy is:

- pushes to the configured dev branch publish or update the rolling prerelease tagged `pvs8.1-dev`
- pushes to the configured stable branch do not publish a release by themselves
- pushing a sequential tag such as `pvs8.1.1`, `pvs8.1.2`, or `pvs8.1.3` publishes a stable release when the tagged commit is contained in the configured stable branch
- stable releases are explicitly marked as the latest GitHub Release
- tag names outside the configured `pvs<version>.<positive-integer>` series are rejected by the release policy
- the `publish-release` job in [.github/workflows/release-builds.yml](./workflows/release-builds.yml) is the only job that mutates GitHub Releases
- the GitHub Releases page publishes the standalone platform tarballs for successful builds; macOS notarized `.pkg` assets are published in addition to those tarballs when signing and notarization are enabled
- each stable or dev asset family is reconciled once in that final publish job, so the release keeps only the latest asset for each platform/package kind

The normal promotion sequence is to merge `dev` into `master`, create the next
`pvs8.1.N` tag on that master commit, and push the tag. Development then
continues on `dev`, whose moving `pvs8.1-dev` prerelease is updated by the next
successful dev build.

For example, after the release merge is present on `master`:

```sh
git switch master
git pull --ff-only origin master
git tag -a pvs8.1.1 -m "PVS 8.1.1"
git push origin pvs8.1.1
git switch dev
```

## Which Artifact To Distribute

If the goal is to minimize Gatekeeper friction for end users, distribute the notarized `.pkg` artifact when one is available. The standalone platform tarballs are also published on the GitHub Releases page for successful builds.

Internal GitHub Actions artifacts remain date-stamped as
`pvs-<branch>-<date>-<os>-<arch>.tgz` or `.pkg` so concurrent CI runs cannot
collide. The final GitHub Release assets use stable names:

- `pvs-linux-x86_64.tgz`
- `pvs-linux-aarch64.tgz`
- `pvs-macos-arm64.tgz`
- `pvs-macos-x86_64.tgz`
- `pvs-macos-arm64.pkg`, when signing and notarization are enabled
- `pvs-macos-x86_64.pkg`, when signing and notarization are enabled

The current release flow vendors any non-system dylib dependencies discovered
in the packaged runtime directory so the shipped bundle does not reach back
into Homebrew on an end user's machine. The `.pkg` path then signs those Mach-O
payload files, signs the installer package, and notarizes that packaged
distribution.

The stable names provide durable download URLs. For example:

- latest official Linux x86_64: `https://github.com/SRI-CSL/PVS/releases/latest/download/pvs-linux-x86_64.tgz`
- rolling dev Linux x86_64: `https://github.com/SRI-CSL/PVS/releases/download/pvs8.1-dev/pvs-linux-x86_64.tgz`

Every build writes an ignored `metadata.json` at the source-tree root. Release
bundles contain their own `metadata.json` with the PVS version and target,
UTC build time, Git and commit details, GitHub Actions provenance when
available, toolchain versions, and a SHA-256 manifest of the packaged PVS
native binaries, Lisp cores, dynamic libraries, and ASDF system definitions.
Documentation, examples, headers, and solver input files are not inventoried.
Release auditing requires valid metadata. macOS installer packaging refreshes
the manifest after payload signing so its hashes describe the bytes shipped in
the `.pkg`.

## Release Tracks

- Stable releases use explicit, permanent tags in the `pvs8.1.N` series, and the tagged commit must be contained in `master`.
- Dev builds update the single moving prerelease tag `pvs8.1-dev` and replace its assets in place.
- The UTC build date remains in internal Actions artifact filenames; published GitHub Release assets use stable names.
- Asset cleanup is centralized in the final publish job so old Linux/macOS tarballs and notarized macOS packages are pruned in one pass instead of by the individual builders.

For the SBCL runtime, the packaged bundle now uses:

- `pvs-sbclisp`: a shell launcher
- `pvs-sbclisp-bin`: a shell launcher that sets `SBCL_HOME` and invokes the bundled SBCL
- `pvs-sbclisp.core`: the SBCL core image data file
- `runtime/sbcl/`: the bundled SBCL executable plus its `lib/sbcl/` support tree

This avoids Apple's strict validation failure for a monolithic self-contained SBCL executable with an appended core payload.
The `pvs-sbclisp` launcher also passes an explicit `--dynamic-space-size` to SBCL, defaulting to the build-time `SBCL_SPACE_SIZE` and overridable with `PVS_SBCL_DYNAMIC_SPACE_SIZE`, so the installed runtime does not depend on whatever default heap size the bootstrap-built SBCL binary happens to carry.
When the notarized macOS pkg path signs the bundled `runtime/sbcl/bin/sbcl` executable with Hardened Runtime, it also applies the executable-memory entitlements from [.github/macos-sbcl-entitlements.plist](./macos-sbcl-entitlements.plist): `com.apple.security.cs.allow-jit` and `com.apple.security.cs.allow-unsigned-executable-memory`. The Apple Silicon SBCL runtime can use executable-memory patterns that work in an unsigned/debug bundle but are killed by code-signing enforcement after pkg installation unless those runtime exceptions are present.

## Required Secrets

The `package-macos-arm64` job only runs if all of these secrets are non-empty:

- `MACOS_DEV_ID_APPLICATION_CERT_P12_BASE64`
- `MACOS_DEV_ID_APPLICATION_CERT_PASSWORD`
- `MACOS_DEV_ID_APPLICATION_CERT_NAME`
- `MACOS_DEV_ID_INSTALLER_CERT_P12_BASE64`
- `MACOS_DEV_ID_INSTALLER_CERT_PASSWORD`
- `MACOS_DEV_ID_INSTALLER_CERT_NAME`
- `MACOS_NOTARY_ISSUER_ID`
- `MACOS_NOTARY_KEY_ID`
- `MACOS_NOTARY_API_KEY_P8_BASE64`

If any one of these is missing, the workflow still produces the standalone tarball, but it skips the pkg/notarization job.

## What The Certificate Files Mean

There are two separate pieces involved in the notarized package path:

- `Developer ID Application` `.p12`
  This is the exported `Developer ID Application` identity, including the private key. It is used to sign Mach-O executables and dynamic libraries inside the staged bundle before packaging.

- `Certificates.p12`
  This is the exported `Developer ID Installer` identity, including the private key. It is used to sign the installer package.

- `AuthKey_<KEY_ID>.p8`
  This is the App Store Connect API private key used by `notarytool` to authenticate with Apple's notarization service.

These files are used for different purposes:

- `Developer ID Application` `.p12` = payload signing
- `Developer ID Installer` `.p12` = package signing
- `.p8` = notarization authentication

## Developer ID Application Certificate Setup

The payload signing path uses a `Developer ID Application` certificate.

Typical flow:

1. Create a certificate signing request.
2. Request the `Developer ID Application` certificate from Apple.
3. Download the `.cer`.
4. Import the `.cer` into the login keychain.
5. Verify that the matching identity is available.
6. Export that identity as a `.p12`.

Useful commands:

```bash
security import ~/cert/developerID_application.cer -k ~/Library/Keychains/login.keychain-db
security find-identity -v -p codesigning ~/Library/Keychains/login.keychain-db | grep "Developer ID Application"
```

The identity name you put in `MACOS_DEV_ID_APPLICATION_CERT_NAME` must match the imported identity exactly.

Example:

```text
Developer ID Application: Your Name (TEAMID)
```

## Developer ID Installer Certificate Setup

The package signing path uses a `Developer ID Installer` certificate.

Typical flow:

1. Create a certificate signing request.
2. Request the `Developer ID Installer` certificate from Apple.
3. Download the `.cer`.
4. Import the `.cer` into the login keychain.
5. Verify that the matching identity is available.
6. Export that identity as a `.p12`.

Useful commands:

```bash
security import ~/cert/developerID_installer.cer -k ~/Library/Keychains/login.keychain-db
security find-identity -v -p basic ~/Library/Keychains/login.keychain-db | grep "Developer ID Installer"
```

The identity name you put in `MACOS_DEV_ID_INSTALLER_CERT_NAME` must match the imported identity exactly. To discover the exact name, use the `security find-identity` command above.

Example:

```text
Developer ID Installer: Your Name (TEAMID)
```

## Export The `.p12` Files

After the `Developer ID Application` and `Developer ID Installer` identities appear in the keychain, export each identity as a `.p12` with a password.

Once exported, base64-encode them for GitHub Secrets:

```bash
base64 < ~/cert/DeveloperIDApplication.p12 | tr -d '\n'
base64 < ~/cert/Certificates.p12 | tr -d '\n'
```

## App Store Connect API Key Setup For Notarization

The current workflow uses App Store Connect API key authentication for `notarytool`.

Important:

- Use a `Team Key`
- Do not use an `Individual Key`
- `MACOS_NOTARY_ISSUER_ID` must be the Team Key `Issuer ID` in UUID format

Apple's documentation says individual App Store Connect API keys cannot be used with `notarytool`.

Typical flow:

1. Sign in to App Store Connect.
2. Go to `Users and Access` -> `Integrations` -> `App Store Connect API`.
3. If API access is not enabled, the Account Holder must request access first.
4. Under `Team Keys`, generate a new key.
5. Download the private key file immediately.
6. Save the file somewhere secure, for example `~/cert/AuthKey_<KEY_ID>.p8`.
7. Record the `Key ID`.
8. Record the `Issuer ID`.

Base64-encode the `.p8` file for GitHub Secrets:

```bash
base64 < ~/cert/AuthKey_<KEY_ID>.p8 | tr -d '\n'
```

## Setting Secrets With `gh`

Examples below use `SRI-CSL/PVS`. Replace that if you are configuring a different repository.

```bash
base64 < ~/cert/DeveloperIDApplication.p12 | tr -d '\n' | gh secret set MACOS_DEV_ID_APPLICATION_CERT_P12_BASE64 -R SRI-CSL/PVS
gh secret set MACOS_DEV_ID_APPLICATION_CERT_PASSWORD -R SRI-CSL/PVS
gh secret set MACOS_DEV_ID_APPLICATION_CERT_NAME -R SRI-CSL/PVS --body "Developer ID Application: Your Name (TEAMID)"

base64 < ~/cert/Certificates.p12 | tr -d '\n' | gh secret set MACOS_DEV_ID_INSTALLER_CERT_P12_BASE64 -R SRI-CSL/PVS
gh secret set MACOS_DEV_ID_INSTALLER_CERT_PASSWORD -R SRI-CSLPVS
gh secret set MACOS_DEV_ID_INSTALLER_CERT_NAME -R SRI-CSL/PVS --body "Developer ID Installer: Your Name (TEAMID)"

gh secret set MACOS_NOTARY_ISSUER_ID -R SRI-CSL/PVS
gh secret set MACOS_NOTARY_KEY_ID -R SRI-CSL/PVS
base64 < ~/cert/AuthKey_<KEY_ID>.p8 | tr -d '\n' | gh secret set MACOS_NOTARY_API_KEY_P8_BASE64 -R SRI-CSL/PVS
```

## What Happens Once All Secrets Are Set

Once all nine secrets are present:

1. `build-macos-arm64` builds and uploads the standalone tarball.
2. `package-macos-arm64` runs automatically.
3. The second job:
   - imports the `Developer ID Application` and `Developer ID Installer` identities into a temporary keychain
   - reconstructs the standalone bundle from the tarball artifact
   - signs Mach-O payload files in the staged bundle copy
   - builds a signed installer package
   - submits the package to Apple's notarization service
   - staples the notarization ticket
   - uploads the final `pvs-<branch>-<date>-macos-arm64.pkg` artifact

## Current Authentication Choice

The workflow currently uses App Store Connect API key authentication for notarization:

- `MACOS_NOTARY_ISSUER_ID`
- `MACOS_NOTARY_KEY_ID`
- `MACOS_NOTARY_API_KEY_P8_BASE64`

It does not currently use the alternate Apple ID + app-specific password flow for `notarytool`.

## Troubleshooting

If the pkg job is skipped:

- Check that all nine secrets are present.
- `gh secret list -R SRI-CSL/PVS` will show secret names, but not values.

If the `Developer ID Application` or `Developer ID Installer` identity does not appear after importing the `.cer`:

- The certificate may not be imported yet.
- The matching private key may not be in the current keychain.

If notarization fails:

- Confirm that the `MACOS_NOTARY_KEY_ID` matches the downloaded `.p8` file.
- Confirm that the `MACOS_NOTARY_ISSUER_ID` belongs to the same App Store Connect team key.
- Confirm that `MACOS_NOTARY_ISSUER_ID` is the Issuer ID UUID, not the Team ID, key name, or some other identifier.
- Confirm that the `.p8` file uploaded into `MACOS_NOTARY_API_KEY_P8_BASE64` is the original Team API key downloaded from App Store Connect.
- If the submission reaches Apple and returns `Invalid`, inspect the notarization log. Unsigned or ad hoc-signed Mach-O payload files are a common cause.

If `notarytool` exits with an error like `must be a valid UUID` for `--issuer`:

- The `MACOS_NOTARY_ISSUER_ID` secret is malformed or is not an App Store Connect Team Key Issuer ID.
- Recreate or re-copy the Issuer ID from `Users and Access` -> `Integrations` -> `App Store Connect API` -> `Team Keys`.
- Do not use an Individual API key here; Apple's current documentation says Individual keys cannot be used with `notarytool`.

## References

- [Apple Silicon workflow](./workflows/apple-silicon-build.yml)
- [macOS pkg build helper](./scripts/build-macos-pkg.sh)
- [Apple Developer: Signing your apps for Gatekeeper](https://developer.apple.com/developer-id/)
- [Apple Developer Support: Developer ID](https://developer.apple.com/support/developer-id/)
- [Apple Developer: App Store Connect API setup](https://developer.apple.com/help/app-store-connect/get-started/app-store-connect-api)
- [Apple Developer: Creating API Keys for App Store Connect API](https://developer.apple.com/documentation/appstoreconnectapi/creating-api-keys-for-app-store-connect-api)
- [Apple Developer: TN3147 Migrating to the latest notarization tool](https://developer.apple.com/documentation/technotes/tn3147-migrating-to-the-latest-notarization-tool)

## Installation

Once the pkg is built and downloaded onto a machine, run

```
installer -pkg ./pvs-master-<date>-macos-arm64.pkg -target CurrentUserHomeDirectory
```

to have PVS installed at ~/PVS.
