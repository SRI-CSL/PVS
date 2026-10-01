#!/usr/bin/env bash

set -euo pipefail

usage() {
  cat <<'EOF'
Usage: publish-github-release.sh \
  --tag <tag> \
  --title <title> \
  --asset <path> \
  [--asset-name <name>] \
  [--prune-asset-glob <glob>] \
  [--keep-asset-name <name>] \
  [--notes <text>] \
  [--target <sha>] \
  [--repo <owner/repo>] \
  [--prerelease] \
  [--latest] \
  [--move-tag]

Publishes or updates a GitHub Release and uploads a single asset with a stable
display name. When --move-tag is supplied, the tag is force-moved to --target.
EOF
}

fail() {
  echo "error: $*" >&2
  exit 1
}

tag=
title=
asset=
asset_name=
prune_asset_globs=()
keep_asset_names=()
notes=
target=
repo=${GITHUB_REPOSITORY:-}
prerelease=false
latest=false
move_tag=false
upload_asset=
upload_tmpdir=

while [[ $# -gt 0 ]]; do
  case $1 in
    --tag)
      tag=$2
      shift 2
      ;;
    --title)
      title=$2
      shift 2
      ;;
    --asset)
      asset=$2
      shift 2
      ;;
    --asset-name)
      asset_name=$2
      shift 2
      ;;
    --prune-asset-glob)
      prune_asset_globs+=("$2")
      shift 2
      ;;
    --keep-asset-name)
      keep_asset_names+=("$2")
      shift 2
      ;;
    --notes)
      notes=$2
      shift 2
      ;;
    --target)
      target=$2
      shift 2
      ;;
    --repo)
      repo=$2
      shift 2
      ;;
    --prerelease)
      prerelease=true
      shift
      ;;
    --latest)
      latest=true
      shift
      ;;
    --move-tag)
      move_tag=true
      shift
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

[[ -n ${GH_TOKEN:-} ]] || fail "GH_TOKEN is required"
[[ -n $tag ]] || fail "--tag is required"
[[ -n $title ]] || fail "--title is required"
[[ -n $asset ]] || fail "--asset is required"
[[ -f $asset ]] || fail "asset not found: $asset"
[[ -n $repo ]] || fail "--repo is required or GITHUB_REPOSITORY must be set"

contains_asset_name() {
  local needle=$1
  shift
  local item
  for item in "$@"; do
    [[ $item == "$needle" ]] && return 0
  done
  return 1
}

matches_asset_glob() {
  local asset=$1
  shift
  local glob
  for glob in "$@"; do
    [[ $asset == $glob ]] && return 0
  done
  return 1
}

release_assets_tsv() {
  gh api "repos/$repo/releases/tags/$tag" --jq '.assets[] | "\(.id)\t\(.name)\t\(.label // "")"'
}

delete_release_asset() {
  local asset_id=$1
  local asset_name=$2
  local asset_label=$3
  local current_assets
  local attempt

  for attempt in 1 2 3; do
    if gh api --method DELETE "repos/$repo/releases/assets/$asset_id" >/dev/null 2>&1; then
      return 0
    fi

    if current_assets=$(release_assets_tsv 2>/dev/null); then
      if ! printf '%s\n' "$current_assets" | awk -F '\t' -v id="$asset_id" '$1 == id { found = 1 } END { exit found ? 0 : 1 }'; then
        echo "Skipping already-removed asset ${asset_label:-$asset_name}"
        return 0
      fi
    fi

    if [[ $attempt -lt 3 ]]; then
      sleep 2
    fi
  done

  echo "warning: failed to delete release asset ${asset_label:-$asset_name}; leaving it in the release" >&2
  return 0
}

if [[ -z $target ]]; then
  target=${GITHUB_SHA:-$(git rev-parse HEAD)}
fi

if [[ -z $asset_name ]]; then
  asset_name=$(basename "$asset")
fi

upload_asset=$asset
if [[ $(basename "$asset") != "$asset_name" ]]; then
  upload_tmpdir=$(mktemp -d "${TMPDIR:-/tmp}/pvs-release-asset.XXXXXX")
  cp -f "$asset" "$upload_tmpdir/$asset_name"
  upload_asset="$upload_tmpdir/$asset_name"
fi

if [[ -z $notes ]]; then
  notes=$(cat <<EOF
Automated release published from GitHub Actions.

- Tag: \`$tag\`
- Commit: \`$target\`
EOF
)
fi

notes_file=$(mktemp "${TMPDIR:-/tmp}/pvs-release-notes.XXXXXX")
cleanup() {
  rm -f "$notes_file"
  if [[ -n $upload_tmpdir ]]; then
    rm -rf "$upload_tmpdir"
  fi
}
trap cleanup EXIT
printf '%s\n' "$notes" > "$notes_file"

if [[ $move_tag == true ]]; then
  git config user.name "github-actions[bot]"
  git config user.email "41898282+github-actions[bot]@users.noreply.github.com"
  git tag -f "$tag" "$target"
  git push --force origin "refs/tags/$tag"
fi

release_flags=(--title "$title" --notes-file "$notes_file")
if [[ $prerelease == true ]]; then
  release_flags+=(--prerelease)
fi
if [[ $latest == true ]]; then
  release_flags+=(--latest)
fi

if gh release view "$tag" -R "$repo" >/dev/null 2>&1; then
  gh release edit "$tag" -R "$repo" "${release_flags[@]}"
else
  if ! gh release create "$tag" -R "$repo" --verify-tag "${release_flags[@]}"; then
    gh release edit "$tag" -R "$repo" "${release_flags[@]}"
  fi
fi

gh release upload "$tag" -R "$repo" "$upload_asset" --clobber

if [[ ${#prune_asset_globs[@]} -gt 0 ]]; then
  while IFS=$'\t' read -r asset_id existing_asset existing_label; do
    [[ -n $asset_id ]] || continue
    [[ -n $existing_asset ]] || continue
    if [[ $existing_asset == "$asset_name" ]]; then
      continue
    fi
    if matches_asset_glob "$existing_asset" "${prune_asset_globs[@]}" || { [[ -n $existing_label ]] && matches_asset_glob "$existing_label" "${prune_asset_globs[@]}"; }; then
      delete_release_asset "$asset_id" "$existing_asset" "$existing_label"
    fi
  done < <(release_assets_tsv)
elif [[ ${#keep_asset_names[@]} -gt 0 ]]; then
  while IFS=$'\t' read -r asset_id existing_asset existing_label; do
    [[ -n $asset_id ]] || continue
    [[ -n $existing_asset ]] || continue
    if ! contains_asset_name "$existing_asset" "${keep_asset_names[@]}" && ! { [[ -n $existing_label ]] && contains_asset_name "$existing_label" "${keep_asset_names[@]}"; }; then
      delete_release_asset "$asset_id" "$existing_asset" "$existing_label"
    fi
  done < <(release_assets_tsv)
fi
