#!/usr/bin/env bash

set -euo pipefail

usage() {
  cat <<'EOF'
Usage: prepare-release-build-tree.sh [repo-dir]

Creates a neutral release build tree outside the GitHub runner home and writes
these variables to GITHUB_ENV:
  PVS_RELEASE_BUILD_ROOT
  PVS_RELEASE_WORKDIR
  CI_HOME
  TMPDIR
  HOME
  PVS_RELEASE_FORBIDDEN_PATHS
EOF
}

fail() {
  echo "error: $*" >&2
  exit 1
}

case "${1:-}" in
  -h|--help)
    usage
    exit 0
    ;;
esac

repo_dir=${1:-${GITHUB_WORKSPACE:-$(pwd)}}
repo_dir=$(cd "$repo_dir" && pwd -P)
git -C "$repo_dir" rev-parse --git-dir >/dev/null 2>&1 || fail "not a git checkout: $repo_dir"

parent=${PVS_RELEASE_BUILD_PARENT:-/tmp}
mkdir -p "$parent"
parent=$(cd "$parent" && pwd -P)

run_id=${GITHUB_RUN_ID:-manual}
run_attempt=${GITHUB_RUN_ATTEMPT:-0}
job_name=${GITHUB_JOB:-job}
safe_name=$(printf '%s-%s-%s' "$run_id" "$run_attempt" "$job_name" | tr -c '[:alnum:]._-' '-')

build_root="$parent/pvs-release-build-$safe_name"
source_tree="$build_root/source"
ci_home="$build_root/home"
tmp_dir="$build_root/tmp"
source_sha=$(git -C "$repo_dir" rev-parse HEAD)
runner_home=${HOME:-}

rm -rf "$build_root"
mkdir -p "$ci_home" "$tmp_dir"

echo "Cloning release source $source_sha to $source_tree"
git clone --no-local --quiet "$repo_dir" "$source_tree"
git -C "$source_tree" checkout --quiet --detach "$source_sha"

for required in configure Makefile.in .github/scripts/audit-release-artifact.sh .github/scripts/generate-build-metadata.py .github/scripts/strip-runtime-debug-info.sh; do
  [[ -e $source_tree/$required ]] || fail "prepared source tree is missing $required"
done

if [[ -n ${GITHUB_ENV:-} ]]; then
  {
    echo "PVS_RELEASE_BUILD_ROOT=$build_root"
    echo "PVS_RELEASE_WORKDIR=$source_tree"
    echo "CI_HOME=$ci_home"
    echo "TMPDIR=$tmp_dir"
    echo "HOME=$ci_home"
    echo "PVS_RELEASE_FORBIDDEN_PATHS<<PVS_FORBIDDEN_PATHS"
    printf '%s\n' "$repo_dir"
    [[ -n ${GITHUB_WORKSPACE:-} ]] && (cd "$GITHUB_WORKSPACE" && pwd -P)
    [[ -n ${RUNNER_TEMP:-} ]] && printf '%s\n' "$RUNNER_TEMP"
    [[ -n ${RUNNER_TOOL_CACHE:-} ]] && printf '%s\n' "$RUNNER_TOOL_CACHE"
    [[ -n $runner_home ]] && printf '%s\n' "$runner_home"
    printf '%s\n' "/Users/runner" "/home/runner"
    echo "PVS_FORBIDDEN_PATHS"
  } >> "$GITHUB_ENV"
fi

echo "Prepared release build tree: $source_tree"
