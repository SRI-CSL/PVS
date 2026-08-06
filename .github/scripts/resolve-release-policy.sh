#!/usr/bin/env bash

set -euo pipefail

usage() {
  cat <<'EOF'
Usage: resolve-release-policy.sh [--config <path>]

Resolves whether the current GitHub Actions run should publish a stable release,
a dev prerelease, or no release at all.
EOF
}

fail() {
  echo "error: $*" >&2
  exit 1
}

config_file=.github/release-config.env

while [[ $# -gt 0 ]]; do
  case $1 in
    --config)
      config_file=$2
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

[[ -f $config_file ]] || fail "release config file not found: $config_file"

# shellcheck disable=SC1090
source "$config_file"

sanitize_component() {
  local value=$1
  value=${value//\//-}
  value=${value// /-}
  printf '%s' "$value" | tr -c 'A-Za-z0-9._-' '-' | sed 's/-\{1,\}/-/g;s/^-//;s/-$//'
}

stable_branch=${PVS_RELEASE_STABLE_BRANCH:-}
dev_branch=${PVS_RELEASE_DEV_BRANCH:-}
release_version=${PVS_RELEASE_VERSION:-}

[[ -n $stable_branch ]] || fail "PVS_RELEASE_STABLE_BRANCH must be set in $config_file"
[[ -n $dev_branch ]] || fail "PVS_RELEASE_DEV_BRANCH must be set in $config_file"
[[ -n $release_version ]] || fail "PVS_RELEASE_VERSION must be set in $config_file"
[[ -n ${GITHUB_REF_TYPE:-} ]] || fail "GITHUB_REF_TYPE is required"
[[ -n ${GITHUB_REF_NAME:-} ]] || fail "GITHUB_REF_NAME is required"
[[ -n ${GITHUB_SHA:-} ]] || fail "GITHUB_SHA is required"

publish=false
channel=none
release_tag=
release_title=
prerelease=false
move_tag=false
release_date=$(date -u +%Y%m%d)
artifact_branch=$(sanitize_component "$GITHUB_REF_NAME")
release_version_pattern=${release_version//./\\.}
stable_tag_pattern="^pvs${release_version_pattern}\\.[1-9][0-9]*$"

case ${GITHUB_REF_TYPE} in
  branch)
    if [[ ${GITHUB_REF_NAME} == "$dev_branch" ]]; then
      publish=true
      channel=dev
      release_tag="pvs${release_version}-${artifact_branch}"
      release_title="PVS ${release_version} Development Snapshot"
      prerelease=true
      move_tag=true
    fi
    ;;
  tag)
    if [[ ${GITHUB_REF_NAME} =~ $stable_tag_pattern ]]; then
      fetch_args=(--no-tags)
      if [[ $(git rev-parse --is-shallow-repository 2>/dev/null) == true ]]; then
        fetch_args+=(--unshallow)
      fi
      git fetch "${fetch_args[@]}" origin \
        "refs/heads/${stable_branch}:refs/remotes/origin/${stable_branch}" >/dev/null 2>&1 || {
        fail "unable to fetch stable branch origin/${stable_branch} for tag validation"
      }
      if git merge-base --is-ancestor "$GITHUB_SHA" "refs/remotes/origin/${stable_branch}"; then
        publish=true
        channel=stable
        release_tag=${GITHUB_REF_NAME}
        release_title="PVS ${GITHUB_REF_NAME#pvs}"
        prerelease=false
        move_tag=false
        artifact_branch=$(sanitize_component "$stable_branch")
      fi
    fi
    ;;
esac

if [[ -n ${GITHUB_OUTPUT:-} ]]; then
  {
    echo "stable_branch=$stable_branch"
    echo "dev_branch=$dev_branch"
    echo "release_version=$release_version"
    echo "artifact_branch=$artifact_branch"
    echo "artifact_date=$release_date"
    echo "publish=$publish"
    echo "channel=$channel"
    echo "release_tag=$release_tag"
    echo "release_title=$release_title"
    echo "prerelease=$prerelease"
    echo "move_tag=$move_tag"
    echo "release_date=$release_date"
  } >> "$GITHUB_OUTPUT"
else
  printf 'stable_branch=%s\n' "$stable_branch"
  printf 'dev_branch=%s\n' "$dev_branch"
  printf 'release_version=%s\n' "$release_version"
  printf 'artifact_branch=%s\n' "$artifact_branch"
  printf 'artifact_date=%s\n' "$release_date"
  printf 'publish=%s\n' "$publish"
  printf 'channel=%s\n' "$channel"
  printf 'release_tag=%s\n' "$release_tag"
  printf 'release_title=%s\n' "$release_title"
  printf 'prerelease=%s\n' "$prerelease"
  printf 'move_tag=%s\n' "$move_tag"
  printf 'release_date=%s\n' "$release_date"
fi
