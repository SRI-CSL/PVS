#!/usr/bin/env bash

set -euo pipefail

usage() {
  cat <<'EOF'
Usage: build-macos-pkg.sh \
  --bundle-dir <dir> \
  --output-dir <dir> \
  --pkg-name <name.pkg> \
  --pkg-identifier <identifier> \
  --pkg-version <version> \
  [--install-base </PVS>]

Required environment variables:
  MACOS_INSTALLER_SIGN_IDENTITY

Optional environment variables for notarization:
  MACOS_NOTARY_KEY_FILE
  MACOS_NOTARY_KEY_ID
  MACOS_NOTARY_ISSUER_ID (Team API key issuer UUID)

Optional environment variables for payload signing:
  MACOS_APPLICATION_SIGN_IDENTITY
  MACOS_SIGNING_KEYCHAIN
EOF
}

fail() {
  echo "error: $*" >&2
  exit 1
}

is_uuid() {
  [[ $1 =~ ^[[:xdigit:]]{8}-([[:xdigit:]]{4}-){3}[[:xdigit:]]{12}$ ]]
}

sign_macho_payload() {
  local root=$1
  local identity=$2
  local keychain_path=${3:-}
  local sbcl_entitlements=${4:-}
  local signed_any=false

  while IFS= read -r -d '' path; do
    local file_desc
    file_desc=$(file -b "$path")
    if grep -q 'Mach-O' <<<"$file_desc"; then
      echo "Signing Mach-O payload $path"
      local -a codesign_args=(
        --force
        --options runtime
        --sign "$identity"
        --timestamp
      )
      if [[ -n $keychain_path ]]; then
        codesign_args+=(--keychain "$keychain_path")
      fi
      if [[ -n $sbcl_entitlements ]] \
        && [[ $path == */runtime/sbcl/bin/sbcl ]] \
        && grep -q 'executable' <<<"$file_desc"; then
        echo "Applying SBCL JIT entitlement to $path"
        codesign_args+=(--entitlements "$sbcl_entitlements")
      fi
      codesign "${codesign_args[@]}" "$path"
      signed_any=true
    fi
  done < <(find "$root" -type f -print0)

  if [[ $signed_any == false ]]; then
    echo "No Mach-O payload files found under $root"
  fi
}

bundle_macos_runtime_deps() {
  local bundle_root=$1
  local helper
  local found_any=false

  helper="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)/bundle-macos-runtime-deps.sh"
  [[ -x $helper ]] || fail "runtime dependency helper not found: $helper"

  while IFS= read -r -d '' runtime_dir; do
    echo "Bundling non-system runtime dependencies in $runtime_dir"
    "$helper" --runtime-dir "$runtime_dir"
    found_any=true
  done < <(find "$bundle_root/bin" -type d -name runtime -print0)

  if [[ $found_any == false ]]; then
    echo "No runtime directories found under $bundle_root/bin"
  fi
}

bundle_dir=
output_dir=
pkg_name=
pkg_identifier=
pkg_version=
install_base=/PVS

while [[ $# -gt 0 ]]; do
  case $1 in
    --bundle-dir)
      bundle_dir=$2
      shift 2
      ;;
    --output-dir)
      output_dir=$2
      shift 2
      ;;
    --pkg-name)
      pkg_name=$2
      shift 2
      ;;
    --pkg-identifier)
      pkg_identifier=$2
      shift 2
      ;;
    --pkg-version)
      pkg_version=$2
      shift 2
      ;;
    --install-base)
      install_base=$2
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      fail "unknown argument: $1"
      ;;
  esac
done

[[ -n $bundle_dir ]] || fail "--bundle-dir is required"
[[ -n $output_dir ]] || fail "--output-dir is required"
[[ -n $pkg_name ]] || fail "--pkg-name is required"
[[ -n $pkg_identifier ]] || fail "--pkg-identifier is required"
[[ -n $pkg_version ]] || fail "--pkg-version is required"
[[ -d $bundle_dir ]] || fail "bundle directory not found: $bundle_dir"
[[ -n ${MACOS_INSTALLER_SIGN_IDENTITY:-} ]] || fail "MACOS_INSTALLER_SIGN_IDENTITY is required"

notary_key_file=${MACOS_NOTARY_KEY_FILE:-}
notary_key_id=${MACOS_NOTARY_KEY_ID:-}
notary_issuer_id=${MACOS_NOTARY_ISSUER_ID:-}
signing_keychain=${MACOS_SIGNING_KEYCHAIN:-}
application_sign_entitlements=${MACOS_APPLICATION_SIGN_ENTITLEMENTS_FILE:-}
notarization_enabled=false

if [[ -n $notary_key_file || -n $notary_key_id || -n $notary_issuer_id ]]; then
  [[ -n $notary_key_file ]] || fail "MACOS_NOTARY_KEY_FILE is required when notarization is enabled"
  [[ -f $notary_key_file ]] || fail "MACOS_NOTARY_KEY_FILE does not exist: $notary_key_file"
  [[ -n $notary_key_id ]] || fail "MACOS_NOTARY_KEY_ID is required when notarization is enabled"
  [[ -n $notary_issuer_id ]] || fail "MACOS_NOTARY_ISSUER_ID is required when notarization is enabled"
  is_uuid "$notary_issuer_id" || fail "MACOS_NOTARY_ISSUER_ID must be the App Store Connect Issuer ID UUID for a Team API key; Individual API keys cannot be used with notarytool"
  notarization_enabled=true
fi

mkdir -p "$output_dir"

stage_root=$(mktemp -d "${TMPDIR:-/tmp}/pvs-pkg-stage.XXXXXX")
cleanup() {
  rm -rf "$stage_root"
}
trap cleanup EXIT

mkdir -p "$stage_root$install_base"
cp -R "$bundle_dir" "$stage_root$install_base/"

bundle_macos_runtime_deps "$stage_root$install_base/$(basename "$bundle_dir")"

staged_bundle="$stage_root$install_base/$(basename "$bundle_dir")"
metadata_generator="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)/generate-build-metadata.py"
metadata_file="$staged_bundle/metadata.json"
[[ -f $metadata_generator ]] || fail "metadata generator not found: $metadata_generator"
[[ -f $metadata_file ]] || fail "bundle metadata not found: $metadata_file"

if [[ -n ${MACOS_APPLICATION_SIGN_IDENTITY:-} ]]; then
  echo "Signing staged Mach-O payload with $MACOS_APPLICATION_SIGN_IDENTITY"
  if [[ -n $application_sign_entitlements ]]; then
    [[ -f $application_sign_entitlements ]] || fail "MACOS_APPLICATION_SIGN_ENTITLEMENTS_FILE does not exist: $application_sign_entitlements"
  fi
  sign_macho_payload \
    "$staged_bundle" \
    "$MACOS_APPLICATION_SIGN_IDENTITY" \
    "$signing_keychain" \
    "$application_sign_entitlements"
fi

# Payload signing changes Mach-O bytes, so refresh the embedded manifest before
# pkgbuild seals the installer payload.
python3 "$metadata_generator" \
  --refresh "$metadata_file" \
  --artifact-root "$staged_bundle" \
  --artifact pvs.asd \
  --artifact bin \
  --artifact yices \
  --packaging-format macos-pkg
python3 "$metadata_generator" \
  --validate "$metadata_file" \
  --verify-artifacts "$staged_bundle"

pkg_stem=${pkg_name%.pkg}
unsigned_pkg="$output_dir/$pkg_stem-unsigned.pkg"
signed_pkg="$output_dir/$pkg_name"

echo "Building unsigned package $unsigned_pkg"
pkgbuild \
  --root "$stage_root" \
  --identifier "$pkg_identifier" \
  --version "$pkg_version" \
  --install-location "/" \
  "$unsigned_pkg"

echo "Signing installer package $signed_pkg"
if [[ -n $signing_keychain ]]; then
  productsign \
    --sign "$MACOS_INSTALLER_SIGN_IDENTITY" \
    --keychain "$signing_keychain" \
    "$unsigned_pkg" \
    "$signed_pkg"
else
  productsign \
    --sign "$MACOS_INSTALLER_SIGN_IDENTITY" \
    "$unsigned_pkg" \
    "$signed_pkg"
fi

rm -f "$unsigned_pkg"

echo "Checking installer package signature"
pkgutil --check-signature "$signed_pkg"

notarized=false
if [[ $notarization_enabled == true ]]; then
  submit_plist="$output_dir/$pkg_stem-notary-submit.plist"
  submit_status=
  submission_id=

  echo "Submitting $signed_pkg for notarization"
  xcrun notarytool submit \
    "$signed_pkg" \
    --key "$notary_key_file" \
    --key-id "$notary_key_id" \
    --issuer "$notary_issuer_id" \
    --wait \
    --output-format plist \
    > "$submit_plist"

  submission_id=$(plutil -extract id raw -o - "$submit_plist")
  submit_status=$(plutil -extract status raw -o - "$submit_plist")

  if [[ $submit_status != Accepted ]]; then
    echo "Notarization completed with status: $submit_status"
    if [[ -n $submission_id ]]; then
      echo "Fetching notarization log for submission $submission_id"
      xcrun notarytool log \
        "$submission_id" \
        "$output_dir/$pkg_stem-notary-log.json" \
        --key "$notary_key_file" \
        --key-id "$notary_key_id" \
        --issuer "$notary_issuer_id" || true
      if [[ -f $output_dir/$pkg_stem-notary-log.json ]]; then
        cat "$output_dir/$pkg_stem-notary-log.json"
      fi
    fi
    fail "notarization failed for $signed_pkg"
  fi

  echo "Stapling notarization ticket"
  xcrun stapler staple "$signed_pkg"
  notarized=true
fi

echo "Assessing installer package"
spctl -a -t install -vv "$signed_pkg"

if [[ -n ${GITHUB_OUTPUT:-} ]]; then
  {
    echo "pkg_path=$signed_pkg"
    echo "pkg_notarized=$notarized"
    echo "pkg_install_path=$install_base/$(basename "$bundle_dir")"
  } >> "$GITHUB_OUTPUT"
fi

echo "Created installer package: $signed_pkg"
