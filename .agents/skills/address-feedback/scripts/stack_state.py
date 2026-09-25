#!/usr/bin/env python3
"""Inspect and wait for native GitHub pull-request stack changes."""

from __future__ import annotations

import argparse
import json
import re
import sys
import time
from pathlib import Path
from typing import Any
from urllib.parse import quote

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "delivery-wait" / "scripts"))

from kgr_github import (  # noqa: E402
    Gh,
    Metrics,
    StateLock,
    TransientError,
    WaitError,
    default_state_path,
    emit,
    parse_time,
    positive_interval,
    result_envelope,
    stable_digest,
    wait_for_transition,
)


def repo_parts(repo: str) -> tuple[str, str]:
    pieces = repo.split("/", 1)
    if len(pieces) != 2 or not all(pieces):
        raise WaitError("--repo must be OWNER/REPO")
    return pieces[0], pieces[1]


def positive_stack_number(value: int) -> int:
    if value <= 0:
        raise WaitError("--stack must be a positive native stack number")
    return value


def required_text(value: Any, label: str) -> str:
    if not isinstance(value, str) or not value:
        raise WaitError(f"native stack response is missing {label}")
    return value


def parse_stack(raw: Any, stack_number: int) -> dict[str, Any]:
    if not isinstance(raw, dict):
        raise WaitError("native stack response is not an object")
    if raw.get("number") != stack_number:
        raise WaitError(
            f"requested native stack {stack_number}, observed {raw.get('number')}"
        )

    base = raw.get("base")
    if not isinstance(base, dict):
        raise WaitError("native stack response is missing base")
    pulls = raw.get("pull_requests")
    if not isinstance(pulls, list) or len(pulls) < 2:
        raise WaitError("native stack must contain at least two pull requests")

    members = []
    seen: set[int] = set()
    for position, pull in enumerate(pulls):
        if not isinstance(pull, dict):
            raise WaitError(f"native stack member {position} is not an object")
        number = pull.get("number")
        if not isinstance(number, int) or number <= 0 or number in seen:
            raise WaitError(f"native stack member {position} has an invalid PR number")
        seen.add(number)
        head = pull.get("head")
        if not isinstance(head, dict):
            raise WaitError(f"native stack member PR #{number} is missing head")
        members.append(
            {
                "position": position,
                "pr": number,
                "state": required_text(pull.get("state"), f"PR #{number} state"),
                "draft": bool(pull.get("draft")),
                "merged": bool(pull.get("merged_at")),
                "headRef": required_text(head.get("ref"), f"PR #{number} head ref"),
                "head": required_text(head.get("sha"), f"PR #{number} head SHA"),
            }
        )

    return {
        "stack": stack_number,
        "id": raw.get("id"),
        "nodeId": raw.get("node_id"),
        "url": raw.get("url"),
        "open": bool(raw.get("open")),
        "base": {
            "ref": required_text(base.get("ref"), "base ref"),
        },
        "members": members,
    }


def base_head(gh: Gh, repo: str, ref: str) -> str:
    raw = gh.rest(f"repos/{repo}/git/ref/heads/{quote(ref, safe='/')}")
    if not isinstance(raw, dict) or raw.get("ref") != f"refs/heads/{ref}":
        raise WaitError("native stack base lookup returned a different ref")
    obj = raw.get("object")
    if (not isinstance(obj, dict) or obj.get("type") != "commit"
            or not isinstance(obj.get("sha"), str)
            or not re.fullmatch(r"[0-9a-f]{40}", obj["sha"])):
        raise WaitError("native stack base lookup is missing an exact commit SHA")
    return obj["sha"]


def stack_snapshot(gh: Gh, repo: str, stack_number: int) -> dict[str, Any]:
    repo_parts(repo)
    endpoint = f"repos/{repo}/stacks/{stack_number}"
    first = parse_stack(gh.rest(endpoint), stack_number)
    # The live Stacks API supplies base.ref without base.sha. Resolve the ref
    # independently, and bracket both reads so a changing packet is not accepted.
    before = base_head(gh, repo, first["base"]["ref"])
    current = parse_stack(gh.rest(endpoint), stack_number)
    after = base_head(gh, repo, current["base"]["ref"])
    if first != current or before != after:
        raise TransientError("native stack or base changed during observation; retry the complete snapshot")
    current["base"]["sha"] = after
    return current


def load_expected(path: Path) -> dict[str, Any]:
    try:
        value = json.loads(path.read_text())
    except (OSError, json.JSONDecodeError) as error:
        raise WaitError(f"cannot read expected stack snapshot {path}: {error}") from error
    if isinstance(value, dict) and isinstance(value.get("observation"), dict):
        value = value["observation"]
    if not isinstance(value, dict) or not isinstance(value.get("members"), list):
        raise WaitError("expected stack snapshot needs a members array")
    seen: set[int] = set()
    for position, member in enumerate(value["members"]):
        if not isinstance(member, dict):
            raise WaitError(f"expected stack member {position} is not an object")
        number = member.get("pr")
        head = member.get("head")
        if not isinstance(number, int) or number <= 0 or number in seen:
            raise WaitError(f"expected stack member {position} has an invalid PR number")
        if not isinstance(head, str) or not head:
            raise WaitError(f"expected stack member PR #{number} has an invalid head")
        seen.add(number)
    return value


def compare_snapshots(expected: dict[str, Any], current: dict[str, Any]) -> dict[str, Any]:
    expected_members = expected.get("members") or []
    current_members = current.get("members") or []
    expected_order = [item.get("pr") for item in expected_members]
    current_order = [item.get("pr") for item in current_members]
    expected_by_pr = {item.get("pr"): item for item in expected_members}
    current_by_pr = {item.get("pr"): item for item in current_members}

    added = [number for number in current_order if number not in expected_by_pr]
    removed = [number for number in expected_order if number not in current_by_pr]
    head_changes = [
        {
            "pr": number,
            "before": expected_by_pr[number].get("head"),
            "after": current_by_pr[number].get("head"),
        }
        for number in expected_order
        if number in current_by_pr
        and expected_by_pr[number].get("head") != current_by_pr[number].get("head")
    ]
    state_changes = []
    for number in expected_order:
        if number not in current_by_pr:
            continue
        before = expected_by_pr[number]
        after = current_by_pr[number]
        changed = {
            key: {"before": before.get(key), "after": after.get(key)}
            for key in ("state", "draft", "merged")
            if before.get(key) != after.get(key)
        }
        if changed:
            state_changes.append({"pr": number, "changes": changed})

    base_changed = expected.get("base") != current.get("base")
    stack_changed = any(
        expected.get(key) != current.get(key) for key in ("stack", "id", "nodeId", "open")
    )
    append_only = (
        not base_changed
        and not stack_changed
        and len(current_members) >= len(expected_members)
        and all(
            current_members[index].get("pr") == member.get("pr")
            and current_members[index].get("head") == member.get("head")
            for index, member in enumerate(expected_members)
        )
        and not removed
        and not head_changes
    )

    first_invalidated: int | None = None
    if base_changed or stack_changed:
        first_invalidated = 0
    else:
        for index in range(min(len(expected_members), len(current_members))):
            before = expected_members[index]
            after = current_members[index]
            if before.get("pr") != after.get("pr") or before.get("head") != after.get("head"):
                first_invalidated = index
                break
        if first_invalidated is None and len(current_members) < len(expected_members):
            first_invalidated = len(current_members)

    identical = (
        not base_changed
        and not stack_changed
        and expected_order == current_order
        and not head_changes
        and not state_changes
    )
    return {
        "identical": identical,
        "appendOnly": append_only and len(current_members) > len(expected_members),
        "added": added,
        "removed": removed,
        "orderBefore": expected_order,
        "orderAfter": current_order,
        "headChanges": head_changes,
        "stateChanges": state_changes,
        "baseChanged": base_changed,
        "stackChanged": stack_changed,
        "firstInvalidatedPosition": first_invalidated,
    }


def inspect_classification(
    expected: dict[str, Any] | None, observation: dict[str, Any]
) -> tuple[str, str]:
    if expected is None:
        return "observed", "native stack snapshot captured"
    comparison = compare_snapshots(expected, observation)
    observation["comparison"] = comparison
    if comparison["identical"]:
        return "satisfied", "native stack identity, topology, and exact heads are unchanged"
    if comparison["firstInvalidatedPosition"] is not None:
        return (
            "invalidated",
            f"stack evidence is invalid from position {comparison['firstInvalidatedPosition']}",
        )
    return "changed", "native stack gained members or changed pull-request state"


def wait_classification(
    expected: dict[str, Any], observation: dict[str, Any]
) -> tuple[str, str]:
    state, reason = inspect_classification(expected, observation)
    if state == "satisfied":
        return "waiting", "native stack is unchanged"
    return state, reason


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser()
    subparsers = result.add_subparsers(dest="command", required=True)
    for name in ("inspect", "wait"):
        command = subparsers.add_parser(name)
        command.add_argument("--repo", required=True)
        command.add_argument("--stack", type=int, required=True)
        command.add_argument("--expect", type=Path)
        command.add_argument("--json", action="store_true")
        if name == "wait":
            command.add_argument("--deadline", required=True)
            command.add_argument("--interval", type=float, default=30.0)
            command.add_argument("--state", type=Path)
    return result


def main() -> int:
    args = parser().parse_args()
    metrics = Metrics(time.monotonic())
    identity = {"repo": args.repo, "stack": args.stack}
    try:
        args.stack = positive_stack_number(args.stack)
        expected = load_expected(args.expect) if args.expect else None
        if expected is not None:
            identity["expectedDigest"] = stable_digest(expected)
        gh = Gh(metrics)
        observe = lambda: stack_snapshot(gh, args.repo, args.stack)
        if args.command == "inspect":
            observation = observe()
            metrics.observations += 1
            state, reason = inspect_classification(expected, observation)
            output = result_envelope(
                "stack", state, identity, observation, metrics, reason
            )
        else:
            if expected is None:
                raise WaitError("stack wait requires --expect")
            args.interval = positive_interval(args.interval)
            state_path = args.state or default_state_path("stack", identity)
            with StateLock(state_path):
                output = wait_for_transition(
                    kind="stack",
                    identity=identity,
                    observe=observe,
                    classify=lambda value: wait_classification(expected, value),
                    state_path=state_path,
                    deadline=parse_time(args.deadline),
                    interval=args.interval,
                    metrics=metrics,
                    transition_key=lambda value: value,
                    deadline_result=("pending", "deadline reached without a verified stack transition"),
                    change_precedes_terminal=True,
                )
        emit(output, args.json)
        return 0
    except WaitError as error:
        output = result_envelope(
            f"stack-{args.command}",
            "operational-error",
            identity,
            {},
            metrics,
            str(error),
        )
        emit(output, getattr(args, "json", False))
        return 2


if __name__ == "__main__":
    sys.exit(main())
