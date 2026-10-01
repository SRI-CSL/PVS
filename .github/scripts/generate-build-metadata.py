#!/usr/bin/env python3
"""Generate and validate provenance metadata for PVS builds."""

import argparse
import datetime as dt
import hashlib
import json
import os
import platform
import re
import subprocess
import sys
import tempfile
import urllib.parse
from pathlib import Path


SCHEMA_VERSION = 1
SHA256_RE = re.compile(r"^[0-9a-f]{64}$")
DYNAMIC_LIBRARY_RE = re.compile(r"(?:\.dylib$|\.so(?:\.|$))")
NATIVE_BINARY_MAGICS = (
    b"\x7fELF",
    b"\xfe\xed\xfa\xce",
    b"\xfe\xed\xfa\xcf",
    b"\xce\xfa\xed\xfe",
    b"\xcf\xfa\xed\xfe",
    b"\xca\xfe\xba\xbe",
    b"\xbe\xba\xfe\xca",
    b"\xca\xfe\xba\xbf",
    b"\xbf\xba\xfe\xca",
    b"MZ",
)


def utc_now():
    return dt.datetime.now(dt.timezone.utc).replace(microsecond=0)


def iso_utc(value):
    return value.isoformat().replace("+00:00", "Z")


def run(command, cwd=None):
    try:
        result = subprocess.run(
            command,
            cwd=str(cwd) if cwd else None,
            check=False,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            text=True,
        )
    except OSError:
        return None
    if result.returncode != 0:
        return None
    return result.stdout.strip()


def first_line(command, cwd=None):
    output = run(command, cwd=cwd)
    return output.splitlines()[0] if output else None


def git_value(source_root, *arguments):
    return run(["git", "-C", str(source_root), *arguments])


def sanitize_remote(remote):
    if not remote:
        return None
    if remote.startswith("git@") and ":" in remote:
        return remote
    parsed = urllib.parse.urlsplit(remote)
    if parsed.scheme not in ("http", "https", "ssh", "git") or not parsed.hostname:
        return None
    username = "git@" if parsed.scheme == "ssh" and parsed.username == "git" else ""
    port = ":{}".format(parsed.port) if parsed.port else ""
    return urllib.parse.urlunsplit(
        (parsed.scheme, "{}{}{}".format(username, parsed.hostname, port), parsed.path, "", "")
    )


def git_metadata(source_root):
    head = git_value(source_root, "rev-parse", "HEAD")
    if not head:
        return {"available": False}

    status = git_value(source_root, "status", "--porcelain=v1", "--untracked-files=all") or ""
    status_lines = status.splitlines()
    tracked_changes = sum(1 for line in status_lines if not line.startswith("??"))
    untracked_files = sum(1 for line in status_lines if line.startswith("??"))
    branch = git_value(source_root, "symbolic-ref", "--quiet", "--short", "HEAD")
    tags_text = git_value(source_root, "tag", "--points-at", "HEAD") or ""
    parents_text = git_value(source_root, "show", "-s", "--format=%P", "HEAD") or ""

    commit = {
        "sha": head,
        "short_sha": git_value(source_root, "rev-parse", "--short=12", "HEAD"),
        "tree_sha": git_value(source_root, "show", "-s", "--format=%T", "HEAD"),
        "parent_shas": parents_text.split() if parents_text else [],
        "subject": git_value(source_root, "show", "-s", "--format=%s", "HEAD"),
        "author": {
            "name": git_value(source_root, "show", "-s", "--format=%an", "HEAD"),
            "email": git_value(source_root, "show", "-s", "--format=%ae", "HEAD"),
            "date": git_value(source_root, "show", "-s", "--format=%aI", "HEAD"),
        },
        "committer": {
            "name": git_value(source_root, "show", "-s", "--format=%cn", "HEAD"),
            "email": git_value(source_root, "show", "-s", "--format=%ce", "HEAD"),
            "date": git_value(source_root, "show", "-s", "--format=%cI", "HEAD"),
        },
        "signature": {
            "status": git_value(source_root, "show", "-s", "--format=%G?", "HEAD"),
            "signer": git_value(source_root, "show", "-s", "--format=%GS", "HEAD") or None,
            "key": git_value(source_root, "show", "-s", "--format=%GK", "HEAD") or None,
        },
    }

    return {
        "available": True,
        "version": first_line(["git", "--version"]),
        "branch": branch,
        "detached": branch is None,
        "describe": git_value(source_root, "describe", "--tags", "--always", "--dirty"),
        "tags_at_commit": sorted(tags_text.splitlines()),
        "is_shallow": git_value(source_root, "rev-parse", "--is-shallow-repository") == "true",
        "is_dirty": bool(status_lines),
        "tracked_changes": tracked_changes,
        "untracked_files": untracked_files,
        "remote": sanitize_remote(git_value(source_root, "config", "--get", "remote.origin.url")),
        "commit": commit,
    }


def github_metadata():
    if os.environ.get("GITHUB_ACTIONS") != "true":
        return None
    server = os.environ.get("GITHUB_SERVER_URL", "https://github.com")
    repository = os.environ.get("GITHUB_REPOSITORY")
    run_id = os.environ.get("GITHUB_RUN_ID")
    run_url = None
    if repository and run_id:
        run_url = "{}/{}/actions/runs/{}".format(server.rstrip("/"), repository, run_id)
    return {
        "provider": "github-actions",
        "repository": repository,
        "repository_url": "{}/{}".format(server.rstrip("/"), repository) if repository else None,
        "workflow": os.environ.get("GITHUB_WORKFLOW"),
        "workflow_ref": os.environ.get("GITHUB_WORKFLOW_REF"),
        "workflow_sha": os.environ.get("GITHUB_WORKFLOW_SHA"),
        "run_id": run_id,
        "run_number": os.environ.get("GITHUB_RUN_NUMBER"),
        "run_attempt": os.environ.get("GITHUB_RUN_ATTEMPT"),
        "run_url": run_url,
        "job": os.environ.get("GITHUB_JOB"),
        "event": os.environ.get("GITHUB_EVENT_NAME"),
        "actor": os.environ.get("GITHUB_ACTOR"),
        "ref": os.environ.get("GITHUB_REF"),
        "ref_name": os.environ.get("GITHUB_REF_NAME"),
        "ref_type": os.environ.get("GITHUB_REF_TYPE"),
        "head_ref": os.environ.get("GITHUB_HEAD_REF") or None,
        "base_ref": os.environ.get("GITHUB_BASE_REF") or None,
        "sha": os.environ.get("GITHUB_SHA"),
        "runner_os": os.environ.get("RUNNER_OS"),
        "runner_arch": os.environ.get("RUNNER_ARCH"),
    }


def parse_platform(pvs_platform):
    architecture, separator, operating_system = pvs_platform.partition("-")
    return {
        "pvs_platform": pvs_platform,
        "architecture": architecture if separator else platform.machine(),
        "operating_system": operating_system if separator else platform.system(),
    }


def parse_toolchains(values):
    toolchains = {
        "python": platform.python_version(),
        "git": first_line(["git", "--version"]),
        "make": first_line(["make", "--version"]),
        "c_compiler": first_line([os.environ.get("CC", "cc"), "--version"]),
    }
    for value in values:
        name, separator, version = value.partition("=")
        if separator and name and version:
            toolchains[name] = version
    return {key: value for key, value in toolchains.items() if value}


def sha256_file(path):
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for block in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


def artifact_entry(path, artifact_root, component):
    relative_path = path.relative_to(artifact_root).as_posix()
    stat_result = path.lstat()
    entry = {
        "path": relative_path,
        "component": component,
        "mode": "{:04o}".format(stat_result.st_mode & 0o7777),
        "size_bytes": stat_result.st_size,
    }
    if path.is_symlink():
        entry.update({"type": "symlink", "target": os.readlink(str(path))})
    elif path.is_file():
        entry.update({"type": "file", "sha256": sha256_file(path)})
    else:
        entry["type"] = "other"
    return entry


def is_native_binary(path):
    try:
        with path.open("rb") as stream:
            prefix = stream.read(4)
    except OSError:
        return False
    return any(prefix.startswith(magic) for magic in NATIVE_BINARY_MAGICS)


def is_manifest_artifact(path, artifact_root, allow_asdf=False):
    inspected_path = path
    if path.is_symlink():
        try:
            inspected_path = path.resolve(strict=True)
            inspected_path.relative_to(artifact_root.resolve())
        except (OSError, ValueError):
            return False
    if not inspected_path.is_file():
        return False
    name = inspected_path.name
    return (
        (allow_asdf and name.endswith(".asd"))
        or name.endswith(".core")
        or DYNAMIC_LIBRARY_RE.search(name) is not None
        or is_native_binary(inspected_path)
    )


def artifact_paths(candidate, artifact_root):
    if candidate.is_symlink() or candidate.is_file():
        if is_manifest_artifact(candidate, artifact_root, allow_asdf=True):
            yield candidate
        return
    if not candidate.is_dir():
        return
    for directory, dirnames, filenames in os.walk(str(candidate), followlinks=False):
        directory_path = Path(directory)
        for dirname in list(dirnames):
            path = directory_path / dirname
            if path.is_symlink():
                if is_manifest_artifact(path, artifact_root):
                    yield path
                dirnames.remove(dirname)
        for filename in filenames:
            path = directory_path / filename
            if is_manifest_artifact(path, artifact_root):
                yield path


def artifact_manifest(artifact_root, candidates, generated_at):
    artifact_root = Path(os.path.abspath(str(artifact_root)))
    entries_by_path = {}
    for value in candidates:
        candidate = Path(value)
        if not candidate.is_absolute():
            candidate = artifact_root / candidate
        candidate = Path(os.path.abspath(str(candidate)))
        try:
            component = candidate.relative_to(artifact_root).as_posix()
        except ValueError:
            raise ValueError("artifact is outside the artifact root: {}".format(value))
        for path in artifact_paths(candidate, artifact_root):
            entry = artifact_entry(path, artifact_root, component)
            entries_by_path[entry["path"]] = entry
    return {
        "generated_at": generated_at,
        "hash_algorithm": "sha256",
        "selection": [
            "native-binary",
            "dynamic-library",
            "lisp-core",
            "asdf-system-definition",
        ],
        "components": sorted(set(candidates)),
        "entries": [entries_by_path[path] for path in sorted(entries_by_path)],
    }


def source_uri(git, github):
    if github and github.get("repository_url"):
        return github["repository_url"] + ".git"
    return git.get("remote") if git.get("available") else None


def provenance(git, github):
    commit_sha = git.get("commit", {}).get("sha") if git.get("available") else None
    builder_id = "local"
    if github:
        builder_id = github.get("run_url") or github.get("workflow_ref") or "github-actions"
    return {
        "builder": {"id": builder_id},
        "source": {
            "type": "git",
            "uri": source_uri(git, github),
            "digest": {"sha1": commit_sha} if commit_sha else None,
            "ref": github.get("ref") if github else git.get("branch"),
        },
    }


def source_date_metadata():
    raw_value = os.environ.get("SOURCE_DATE_EPOCH")
    if not raw_value:
        return None
    try:
        epoch = int(raw_value)
        timestamp = iso_utc(dt.datetime.fromtimestamp(epoch, tz=dt.timezone.utc))
    except (ValueError, OverflowError, OSError):
        return {"value": raw_value, "valid": False}
    return {"value": epoch, "timestamp": timestamp, "valid": True}


def write_json(path, data):
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    descriptor, temporary_name = tempfile.mkstemp(prefix=path.name + ".", dir=str(path.parent))
    try:
        with os.fdopen(descriptor, "w", encoding="utf-8") as stream:
            json.dump(data, stream, indent=2, sort_keys=True)
            stream.write("\n")
        os.chmod(temporary_name, 0o644)
        os.replace(temporary_name, str(path))
    except BaseException:
        try:
            os.unlink(temporary_name)
        except OSError:
            pass
        raise


def validate(data):
    if not isinstance(data, dict):
        return ["metadata document must be a JSON object"]
    errors = []
    if data.get("schema_version") != SCHEMA_VERSION:
        errors.append("schema_version must be {}".format(SCHEMA_VERSION))
    for key in ("project", "build", "git", "provenance", "artifacts"):
        if not isinstance(data.get(key), dict):
            errors.append("{} must be an object".format(key))
    project = data.get("project", {}) if isinstance(data.get("project"), dict) else {}
    if not project.get("name") or not project.get("version"):
        errors.append("project name and version are required")
    git = data.get("git", {}) if isinstance(data.get("git"), dict) else {}
    commit = git.get("commit", {}) if isinstance(git.get("commit"), dict) else {}
    if git.get("available") and not commit.get("sha"):
        errors.append("git.commit.sha is required when git metadata is available")
    artifacts = data.get("artifacts", {}) if isinstance(data.get("artifacts"), dict) else {}
    components = artifacts.get("components")
    if not isinstance(components, list) or not components:
        errors.append("artifacts.components must be a non-empty array")
    elif any(not isinstance(component, str) or not component for component in components):
        errors.append("every artifact component must be a non-empty string")
    entries = artifacts.get("entries")
    if not isinstance(entries, list) or not entries:
        errors.append("artifacts.entries must be a non-empty array")
    else:
        seen = set()
        for index, entry in enumerate(entries):
            path = entry.get("path") if isinstance(entry, dict) else None
            if not isinstance(path, str) or not path:
                errors.append("artifact {} has no path".format(index))
                continue
            if path in seen:
                errors.append("duplicate artifact path: {}".format(path))
            seen.add(path)
            if entry.get("type") == "file":
                digest = entry.get("sha256")
                if not isinstance(digest, str) or not SHA256_RE.match(digest):
                    errors.append("artifact {} has an invalid SHA-256 digest".format(path))
    return errors


def verify_artifacts(data, artifact_root):
    artifacts = data["artifacts"]
    actual = artifact_manifest(artifact_root, artifacts["components"], artifacts["generated_at"])
    expected_entries = artifacts["entries"]
    actual_by_path = {entry["path"]: entry for entry in actual["entries"]}
    expected_by_path = {entry["path"]: entry for entry in expected_entries}
    errors = []
    for path in sorted(set(expected_by_path) - set(actual_by_path)):
        errors.append("manifest artifact is missing: {}".format(path))
    for path in sorted(set(actual_by_path) - set(expected_by_path)):
        errors.append("artifact is absent from the manifest: {}".format(path))
    for path in sorted(set(expected_by_path) & set(actual_by_path)):
        if expected_by_path[path] != actual_by_path[path]:
            errors.append("artifact does not match the manifest: {}".format(path))
    return errors


def build_metadata(args):
    now = utc_now()
    generated_at = iso_utc(now)
    source_root = Path(args.source_root).resolve()
    github = github_metadata()
    git = git_metadata(source_root)
    build_id = "{}-{}".format(
        git.get("commit", {}).get("short_sha") or "unknown", int(now.timestamp())
    )
    if github and github.get("run_id"):
        build_id = "github-{}-{}-{}".format(
            github["run_id"], github.get("run_attempt") or "1", github.get("job") or "job"
        )
    return {
        "schema_version": SCHEMA_VERSION,
        "project": {"name": args.package, "version": args.version},
        "build": {
            "id": build_id,
            "generated_at": generated_at,
            "generated_at_epoch_seconds": int(now.timestamp()),
            "source_date_epoch": source_date_metadata(),
            "target": parse_platform(args.platform),
            "host": {
                "operating_system": platform.system(),
                "release": platform.release(),
                "architecture": platform.machine(),
            },
            "toolchains": parse_toolchains(args.toolchain),
        },
        "git": git,
        "ci": github,
        "provenance": provenance(git, github),
        "artifacts": artifact_manifest(args.artifact_root, args.artifact, generated_at),
    }


def refresh_metadata(args):
    metadata_path = Path(args.refresh)
    with metadata_path.open(encoding="utf-8") as stream:
        data = json.load(stream)
    errors = validate(data)
    if errors:
        raise ValueError("cannot refresh invalid metadata: {}".format("; ".join(errors)))
    generated_at = iso_utc(utc_now())
    data["artifacts"] = artifact_manifest(args.artifact_root, args.artifact, generated_at)
    data.setdefault("packaging", []).append(
        {
            "format": args.packaging_format,
            "payload_manifest_refreshed_at": generated_at,
            "ci": github_metadata(),
        }
    )
    write_json(metadata_path, data)


def parse_arguments():
    parser = argparse.ArgumentParser(description=__doc__)
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument("--output", help="write newly generated metadata to this path")
    mode.add_argument("--refresh", help="refresh artifact hashes in an existing metadata file")
    mode.add_argument("--validate", help="validate an existing metadata file")
    parser.add_argument("--source-root", default=".")
    parser.add_argument("--artifact-root", default=".")
    parser.add_argument("--artifact", action="append", default=[])
    parser.add_argument("--package", default="pvs")
    parser.add_argument("--version", default="unknown")
    parser.add_argument("--platform", default=platform.machine() + "-" + platform.system())
    parser.add_argument("--toolchain", action="append", default=[])
    parser.add_argument("--packaging-format", default="unspecified")
    parser.add_argument(
        "--verify-artifacts",
        help="with --validate, verify the manifest against this artifact root",
    )
    return parser.parse_args()


def main():
    args = parse_arguments()
    try:
        if args.validate:
            with Path(args.validate).open(encoding="utf-8") as stream:
                data = json.load(stream)
            errors = validate(data)
            if not errors and args.verify_artifacts:
                errors.extend(verify_artifacts(data, args.verify_artifacts))
            if errors:
                for error in errors:
                    print("error: {}".format(error), file=sys.stderr)
                return 1
            print("Valid PVS build metadata: {}".format(args.validate))
            return 0
        if not args.artifact:
            raise ValueError("at least one --artifact is required")
        if args.refresh:
            refresh_metadata(args)
            print("Refreshed PVS artifact metadata: {}".format(args.refresh))
        else:
            data = build_metadata(args)
            errors = validate(data)
            if errors:
                raise ValueError("generated invalid metadata: {}".format("; ".join(errors)))
            write_json(args.output, data)
            print("Generated PVS build metadata: {}".format(args.output))
        return 0
    except (OSError, ValueError, json.JSONDecodeError) as error:
        print("error: {}".format(error), file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
