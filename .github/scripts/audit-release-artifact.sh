#!/usr/bin/env bash

set -euo pipefail

usage() {
  cat <<'EOF'
Usage: audit-release-artifact.sh --artifact FILE [--path PATH ...] [--foreign-build-root PATH ...]

Extracts a release archive, validates its metadata.json and SHA-256 artifact
manifest, and fails if any forbidden build-machine path appears in the payload.
If --path is omitted, paths are read from PVS_RELEASE_FORBIDDEN_PATHS, one per
line.

If --foreign-build-root is provided, only absolute references to the PVS
runtime foreign libraries under that root are rejected. This catches SBCL saved
shared-object state without rejecting harmless source-debug strings.
EOF
}

fail() {
  echo "error: $*" >&2
  exit 1
}

artifact=
paths=()
foreign_build_roots=()

while [[ $# -gt 0 ]]; do
  case $1 in
    --artifact)
      artifact=${2:-}
      shift 2
      ;;
    --path)
      paths+=("${2:-}")
      shift 2
      ;;
    --foreign-build-root)
      foreign_build_roots+=("${2:-}")
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

[[ -n $artifact ]] || fail "--artifact is required"
[[ -f $artifact ]] || fail "artifact not found: $artifact"

if [[ ${#paths[@]} -eq 0 && -n ${PVS_RELEASE_FORBIDDEN_PATHS:-} ]]; then
  while IFS= read -r path; do
    [[ -n $path ]] && paths+=("$path")
  done <<< "$PVS_RELEASE_FORBIDDEN_PATHS"
fi

[[ ${#paths[@]} -gt 0 ]] || fail "no forbidden paths were provided"

tmpdir=$(mktemp -d "${TMPDIR:-/tmp}/pvs-artifact-audit.XXXXXX")
cleanup() {
  rm -rf "$tmpdir"
}
trap cleanup EXIT HUP INT TERM

case $artifact in
  *.tar.gz|*.tgz)
    tar xzf "$artifact" -C "$tmpdir"
    ;;
  *.tar)
    tar xf "$artifact" -C "$tmpdir"
    ;;
  *)
    fail "unsupported artifact format: $artifact"
    ;;
esac

metadata_generator="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)/generate-build-metadata.py"
[[ -f $metadata_generator ]] || fail "metadata validator not found: $metadata_generator"
metadata_files=()
while IFS= read -r -d '' metadata_file; do
  metadata_files+=("$metadata_file")
done < <(find "$tmpdir" -type f -name metadata.json -print0)
[[ ${#metadata_files[@]} -eq 1 ]] || fail "release artifact must contain exactly one metadata.json; found ${#metadata_files[@]}"
python3 "$metadata_generator" \
  --validate "${metadata_files[0]}" \
  --verify-artifacts "$(dirname "${metadata_files[0]}")"

clean=true
matches_file="$tmpdir/matches"
for forbidden_path in "${paths[@]}"; do
  [[ -n $forbidden_path ]] || continue
  if rg -a -l -F -- "$forbidden_path" "$tmpdir" >"$matches_file"; then
    echo "Forbidden build path found in artifact: $forbidden_path" >&2
    sed -n '1,20p' "$matches_file" >&2
    clean=false
  fi
  rm -f "$matches_file"
done

for build_root in "${foreign_build_roots[@]}"; do
  [[ -n $build_root ]] || continue
  for pattern in \
    "$build_root/src/utils/aarch64-Linux/file_utils.so" \
    "$build_root/src/BDD/aarch64-Linux/mu.so" \
    "$build_root/src/WS1S/aarch64-Linux/ws1s.so" \
    "$build_root/src/utils/ix86_64-Linux/file_utils.so" \
    "$build_root/src/BDD/ix86_64-Linux/mu.so" \
    "$build_root/src/WS1S/ix86_64-Linux/ws1s.so" \
    "$build_root/src/utils/arm-MacOSX/file_utils.dylib" \
    "$build_root/src/BDD/arm-MacOSX/mu.dylib" \
    "$build_root/src/WS1S/arm-MacOSX/ws1s.dylib" \
    "$build_root/src/utils/ix86-MacOSX/file_utils.dylib" \
    "$build_root/src/BDD/ix86-MacOSX/mu.dylib" \
    "$build_root/src/WS1S/ix86-MacOSX/ws1s.dylib"; do
    if rg -a -l -F -- "$pattern" "$tmpdir" >"$matches_file"; then
      echo "Build-time PVS foreign library path found in artifact: $pattern" >&2
      sed -n '1,20p' "$matches_file" >&2
      clean=false
    fi
    rm -f "$matches_file"
  done
done

[[ $clean == true ]] || fail "release artifact contains build-machine paths"

echo "Release artifact has no forbidden build-machine paths: $artifact"
