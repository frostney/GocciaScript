#!/usr/bin/env python3
"""Bind native stack pushes to approved Git objects. Standard library only."""
from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import shlex
import subprocess
import sys
from datetime import datetime, timezone
import uuid

sys.dont_write_bytecode = True  # Keep installed skill trees free of __pycache__.


class GuardError(RuntimeError):
    pass


def encode(value):
    return json.dumps(value, ensure_ascii=False, separators=(",", ":"), allow_nan=False).encode("utf-8")


def digest(value):
    return hashlib.sha256(encode(value)).hexdigest()


def pairs(items):
    result = {}
    for key, value in items:
        if key in result:
            raise GuardError("duplicate JSON key")
        result[key] = value
    return result


def load_json(raw):
    return json.loads(raw, object_pairs_hook=pairs)


def write_once(path, value):
    path = Path(path)
    temporary = path.with_name(path.name + "." + uuid.uuid4().hex + ".tmp")
    try:
        fd = os.open(temporary, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
        with os.fdopen(fd, "wb") as stream:
            stream.write(encode(value))
            stream.flush()
            os.fsync(stream.fileno())
        os.link(temporary, path)
        directory = os.open(path.parent, os.O_RDONLY)
        try:
            os.fsync(directory)
        finally:
            os.close(directory)
    finally:
        temporary.unlink(missing_ok=True)


def read_record(path):
    path = Path(path)
    if path.is_symlink() or not path.is_file():
        raise GuardError("admission is not a regular owned file")
    value = load_json(path.read_bytes())
    if (not isinstance(value, dict) or set(value) != {"schemaVersion", "request", "digest"}
            or type(value["schemaVersion"]) is not int or value["schemaVersion"] != 1
            or value["digest"] != digest(value["request"])):
        raise GuardError("admission record is corrupt")
    return value["request"]


def validate_admission(value):
    if not isinstance(value, dict) or set(value) != {"directory", "remote", "url", "refs"}:
        raise GuardError("invalid admission fields")
    if not isinstance(value["directory"], str) or not Path(value["directory"]).is_absolute():
        raise GuardError("checkout must be an absolute path")
    if not isinstance(value["remote"], str) or not re.fullmatch(r"[A-Za-z0-9][A-Za-z0-9._/-]*", value["remote"]):
        raise GuardError("invalid remote name")
    if not isinstance(value["url"], str) or not value["url"] or any(ord(c) < 32 for c in value["url"]):
        raise GuardError("invalid remote URL")
    if not isinstance(value["refs"], list) or not value["refs"]:
        raise GuardError("admission needs explicit branch refs")
    refs, seen, lengths = [], set(), set()
    for row in value["refs"]:
        if not isinstance(row, dict) or set(row) != {"ref", "source", "expected"}:
            raise GuardError("invalid admitted ref fields")
        ref = row["ref"]
        if (not isinstance(ref, str) or not ref.startswith("refs/heads/") or ref.endswith(("/", "."))
                or any(ord(c) <= 32 or ord(c) == 127 or c in "~^:?*[\\" for c in ref)
                or any(part.startswith(".") or part.endswith(".lock") for part in ref.split("/"))
                or any(part in ref for part in ("..", "@{", "//")) or ref in seen):
            raise GuardError("invalid or duplicate admitted branch ref")
        for name in ("source", "expected"):
            if not isinstance(row[name], str) or not re.fullmatch(r"(?:[0-9a-f]{40}|[0-9a-f]{64})", row[name]):
                raise GuardError("invalid Git object ID")
            lengths.add(len(row[name]))
        if set(row["source"]) == {"0"}:
            raise GuardError("branch deletion is outside this publication helper")
        seen.add(ref)
        refs.append({"ref": ref, "source": row["source"], "expected": row["expected"]})
    if len(lengths) != 1:
        raise GuardError("mixed Git object formats")
    return {"directory": value["directory"], "remote": value["remote"], "url": value["url"], "refs": refs}


def validate_updates(admission, remote, url, text):
    c = validate_admission(admission)
    if remote != c["remote"] or url != c["url"]:
        raise GuardError("remote identity changed")
    allowed = {r["ref"]: r for r in c["refs"]}
    seen, updates = set(), []
    for line in text.split("\n"):
        if not line:
            continue
        parts = line.split(" ")
        if len(parts) != 4:
            raise GuardError("invalid update record")
        local, source, ref, expected = parts
        row = allowed.get(ref)
        if row is None or local != ref or ref in seen:
            raise GuardError("unexpected or duplicated ref update")
        if source != row["source"] or expected != row["expected"]:
            raise GuardError("queued source or advertised remote head differs from admission")
        seen.add(ref)
        updates.append({"ref": ref, "source": source, "expected": expected})
    return updates


def git(directory, *args):
    result = subprocess.run(["git", *args], cwd=directory, stdin=subprocess.DEVNULL, capture_output=True)
    if result.returncode:
        raise GuardError("Git preflight failed: " + args[0])
    return result.stdout.decode("utf-8").strip()


def config_count(environment):
    raw = environment.get("GIT_CONFIG_COUNT", "0")
    if not re.fullmatch(r"0|[1-9][0-9]*", raw) or int(raw) > 1000:
        raise GuardError("invalid inherited Git config count")
    for number in range(int(raw)):
        if any(f"GIT_CONFIG_{key}_{number}" not in environment for key in ("KEY", "VALUE")):
            raise GuardError("incomplete inherited Git configuration")
    return int(raw)


def prepare(value):
    c = validate_admission(value)
    directory = Path(c["directory"])
    if str(directory.resolve(strict=True)) != c["directory"]:
        raise GuardError("checkout path changed")
    if git(directory, "status", "--porcelain"):
        raise GuardError("publication checkout is dirty")
    if git(directory, "remote", "get-url", "--push", c["remote"]) != c["url"]:
        raise GuardError("remote push URL differs from admission")
    object_length = 64 if git(directory, "rev-parse", "--show-object-format") == "sha256" else 40
    for row in c["refs"]:
        git(directory, "check-ref-format", row["ref"])
        if len(row["source"]) != object_length:
            raise GuardError("admission object format differs from repository")
        git(directory, "cat-file", "-e", row["source"] + "^{commit}")
    count = config_count(os.environ)
    git_dir = Path(git(directory, "rev-parse", "--absolute-git-dir"))
    original_hooks = Path(git(directory, "rev-parse", "--path-format=absolute", "--git-path", "hooks"))
    root = git_dir / "kgr-push-guards" / str(uuid.uuid4())
    hooks = root / "hooks"
    hooks.mkdir(parents=True, mode=0o700)
    original_pre_push = None
    if original_hooks.is_dir():
        for entry in original_hooks.iterdir():
            if entry.name == "pre-push":
                if entry.is_file() and os.access(entry, os.X_OK):
                    original_pre_push = str(entry)
            else:
                (hooks / entry.name).symlink_to(entry)
    source = Path(__file__).read_bytes()
    frozen = root / "guard.py"
    with frozen.open("xb") as stream:
        stream.write(source)
        stream.flush()
        os.fsync(stream.fileno())
    request = {"admission": c, "directory": str(root), "originalHooks": str(original_hooks),
               "originalPrePush": original_pre_push, "helperDigest": hashlib.sha256(source).hexdigest(),
               "pythonBinary": str(Path(sys.executable).resolve()), "pythonVersion": sys.version,
               "injectedConfigIndex": count, "originalConfigCount": os.environ.get("GIT_CONFIG_COUNT")}
    config = root / "admission.json"
    write_once(config, {"schemaVersion": 1, "request": request, "digest": digest(request)})
    hook = hooks / "pre-push"
    hook.write_text("#!/bin/sh\nexec " + " ".join(shlex.quote(v) for v in
                    (request["pythonBinary"], str(frozen), "_hook", str(config))) + ' "$@"\n')
    hook.chmod(0o755)
    return {"directory": str(root), "configuration": {"GIT_CONFIG_COUNT": str(count + 1),
            f"GIT_CONFIG_KEY_{count}": "core.hooksPath", f"GIT_CONFIG_VALUE_{count}": str(hooks)}}


def original_hook_environment(config):
    environment = dict(os.environ)
    index = config["injectedConfigIndex"]
    if (environment.get("GIT_CONFIG_COUNT") != str(index + 1)
            or environment.get(f"GIT_CONFIG_KEY_{index}") != "core.hooksPath"
            or environment.get(f"GIT_CONFIG_VALUE_{index}") != str(Path(config["directory"]) / "hooks")):
        raise GuardError("guard configuration environment changed")
    del environment[f"GIT_CONFIG_KEY_{index}"]
    del environment[f"GIT_CONFIG_VALUE_{index}"]
    if config["originalConfigCount"] is None:
        environment.pop("GIT_CONFIG_COUNT", None)
    else:
        environment["GIT_CONFIG_COUNT"] = config["originalConfigCount"]
    return environment


def hook(config_path, remote, url):
    config = read_record(config_path)
    root = Path(config["directory"])
    if (Path(config_path).resolve().parent != root or Path(__file__).resolve() != root / "guard.py"
            or hashlib.sha256(Path(__file__).read_bytes()).hexdigest() != config["helperDigest"]
            or str(Path(sys.executable).resolve()) != config["pythonBinary"] or sys.version != config["pythonVersion"]):
        raise GuardError("frozen guard configuration or runtime changed")
    receipt = {"accepted": False, "admissionDigest": digest(config["admission"]), "pid": os.getpid(),
               "at": datetime.now(timezone.utc).isoformat()}
    try:
        if str(Path.cwd().resolve()) != config["admission"]["directory"]:
            raise GuardError("checkout identity changed")
        limit = sum(2 * len(r["ref"].encode("utf-8")) + len(r["source"]) + len(r["expected"]) + 4
                    for r in config["admission"]["refs"])
        data = sys.stdin.buffer.read(limit + 1)
        if len(data) > limit:
            raise GuardError("update input exceeds admitted scope")
        updates = validate_updates(config["admission"], remote, url, data.decode("utf-8"))
        environment = original_hook_environment(config)
        if config["originalPrePush"]:
            result = subprocess.run([config["originalPrePush"], remote, url], input=data, env=environment)
            if result.returncode:
                raise GuardError("original pre-push hook rejected publication")
        receipt.update(accepted=True, updates=updates)
    except Exception as error:
        receipt["error"] = "Error: Push guard: " + str(error)
        raise
    finally:
        write_once(root / ("check-" + str(uuid.uuid4()) + ".json"), receipt)


# The guard follows the latest gh-stack release (checked at tag v0.2.1, 29a36d3)
# and checks only that the command is gh-stack, not its version: --version prints
# "gh stack version VERSION" on stdout (cmd/root.go:84), and the release notifier
# writes only to stderr (cmd/root.go:56-82). push and submit pass force=true with
# per-branch --force-with-lease and fully qualified refs/heads/B:refs/heads/B
# refspecs, pushing non-atomically after a best-effort tracking fetch
# (cmd/push.go:106-108, cmd/submit.go:204,245); link pushes only arguments naming
# local branches, without force and atomically (cmd/link.go:425-458);
# internal/git/gitops.go:257-290 builds those arguments. No guarded path pushes
# with --no-verify or a + refspec. Pushes run in the current checkout through the
# unscoped executor; the worktree-scoped executor drops GIT_CONFIG but keeps
# GIT_CONFIG_COUNT/KEY/VALUE (internal/git/git.go:49-69). The flags are --auto,
# --open and --remote for submit, --remote for push, and --base, --open and
# --remote for link. link takes a stack or PR number, branch, or PR URL, then one
# or more branch-or-PR arguments (cmd/link.go:27,73); a number naming a stack
# appends to it, otherwise every PR the stack holds must be listed
# (cmd/link.go:170-191,644-671). Its base corrections only warn on failure
# (cmd/link.go:718-761).
GH_STACK_IDENTITY = re.compile(r"gh stack version \S+")
POSITIVE_NUMBER = re.compile(r"[1-9][0-9]*")
# What Go's strconv.Atoi parses; link tries such an argument as a PR number first
# (cmd/link.go:538-551), so a branch named like one is not a safe branch argument.
ATOI_NUMBER = re.compile(r"[+-]?[0-9]+")


def validate_native_command(command, admission):
    if len(command) < 3 or Path(command[0]).name != "gh" or command[1] != "stack":
        raise GuardError("run requires an explicit native gh stack command")
    tail = command[2:]
    if tail == ["submit", "--auto", "--remote", admission["remote"]]:
        return
    if tail == ["push", "--remote", admission["remote"]]:
        return
    # link STACK-OR-PR BRANCH-OR-PR... --remote REMOTE: each later argument is a
    # PR number or an admitted branch that cannot be read as a flag or a number. A number
    # that also names a local branch is pushed, and the hook refuses it unless
    # that branch is admitted.
    admitted = {r["ref"] for r in admission["refs"]}
    targets = tail[1:-2]
    if (len(tail) >= 5 and tail[0] == "link" and tail[-2:] == ["--remote", admission["remote"]]
            and POSITIVE_NUMBER.fullmatch(tail[1]) and len(set(targets)) == len(targets)
            and all(POSITIVE_NUMBER.fullmatch(argument)
                    or (not argument.startswith("-") and not ATOI_NUMBER.fullmatch(argument)
                        and "refs/heads/" + argument in admitted)
                    for argument in targets[1:])):
        return
    raise GuardError("unsupported native publication command; do not bypass hooks")


def main(argv=None):
    if sys.version_info < (3, 11):
        raise GuardError("Python 3.11 or newer is required")
    argv = sys.argv[1:] if argv is None else argv
    if argv and argv[0] == "_hook":
        if len(argv) != 4:
            raise GuardError("invalid hook invocation")
        hook(*argv[1:])
        return 0
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("operation", choices=("prepare", "run"))
    parser.add_argument("--admission", required=True, help="JSON file, or - for stdin; keep outside tracked/untracked work")
    split = argv.index("--") if "--" in argv else len(argv)
    command = argv[split + 1:]
    argv = argv[:split]
    args = parser.parse_args(argv)
    raw = sys.stdin.buffer.read() if args.admission == "-" else Path(args.admission).read_bytes()
    admission = validate_admission(load_json(raw))
    if args.operation == "prepare" and command:
        raise GuardError("prepare does not execute a command")
    if args.operation == "run":
        validate_native_command(command, admission)
        try:
            version = subprocess.run([command[0], "stack", "--version"], capture_output=True, text=True)
        except OSError:
            version = None
        if version is None or version.returncode or not GH_STACK_IDENTITY.fullmatch(version.stdout.strip()):
            raise GuardError("command is not the gh-stack extension")
    prepared = prepare(admission)
    if args.operation == "prepare":
        print(json.dumps(prepared))
        return 0
    result = subprocess.run(command, cwd=admission["directory"], env={**os.environ, **prepared["configuration"]})
    write_once(Path(prepared["directory"]) / "run.json", {"command": command, "exitCode": result.returncode})
    print(json.dumps({"guardDirectory": prepared["directory"], "exitCode": result.returncode}), file=sys.stderr)
    return result.returncode if result.returncode >= 0 else 128 - result.returncode


if __name__ == "__main__":
    try:
        sys.exit(main())
    except Exception as error:
        print("Error: Push guard: " + str(error), file=sys.stderr)
        sys.exit(1)
