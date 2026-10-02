#!/usr/bin/env python3
"""Record the CodeRabbit evidence the adapter reads from public pull requests.

    python3 record_coderabbit.py --out fixtures/coderabbit_recorded.json \
        --since 2026-08-02 owner/repo#12 owner/repo#13 @more-refs.txt

For each pull request it keeps only what `coderabbit_adapter.py` reads:

- CodeRabbit's statuses on every head the branch had (state, description,
  created_at);
- CodeRabbit's reviews (`commit_id`, `submitted_at`, and the lines that
  count findings);
- every version of CodeRabbit's comments from their edit history, each
  reduced to the summary marker, coverage marker, stated-wait sentence and
  HTML block markers, with all other prose dropped;
- other comments that address CodeRabbit, line by line: a line that is only a
  CodeRabbit command keeps it, a line that mentions one inside other text
  keeps it between `[text]` placeholders, and every other line becomes
  `[text]` or stays empty;
- when the pull request was opened, marked ready or draft, closed, reopened
  and merged, and when each head was pushed (its check suites); and
- each head's parents.

It refuses any repository that an anonymous request cannot read as public,
and keeps only pull requests whose CodeRabbit activity all falls on or after
`--since`, because CodeRabbit's behavior and wording change over time. An
existing output file is extended: recorded pull requests inside the window
are kept.
"""

from __future__ import annotations

import argparse
import importlib.util
import json
import re
import subprocess
import sys
import time
import urllib.error
import urllib.request
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
from typing import Any

sys.dont_write_bytecode = True

ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location(
    "coderabbit_adapter", ROOT / "address-feedback" / "scripts" / "coderabbit_adapter.py"
)
assert SPEC and SPEC.loader
ADAPTER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(ADAPTER)

BLOCK_MARKER = re.compile(r"^\s*(?:>\s*)?<!--\s*(?:[\w .:\-]+|final_review_risk_coverage:\{[^{}]*\})\s*-->\s*$")
ABOUT = (
    "Recorded by record_coderabbit.py: only the fields coderabbit_adapter.py reads, from public "
    "pull requests whose CodeRabbit activity all falls on or after `since`."
)
COMMAND = re.compile(r"@coderabbitai\s+[a-z][a-z ]*", re.IGNORECASE)
# Kept whatever wording the adapter reads, so a changed notice stays replayable.
WAIT_SENTENCE = re.compile(r"(?:available in|please wait)\D{0,20}\d", re.IGNORECASE)

PULL_QUERY = """query($owner: String!, $name: String!, $number: Int!, $cursor: String) {
  repository(owner: $owner, name: $name) { pullRequest(number: $number) {
    createdAt headRefName headRefOid baseRefName isDraft state
    timelineItems(first: 100, after: $cursor, itemTypes: [READY_FOR_REVIEW_EVENT,
        CONVERT_TO_DRAFT_EVENT, CLOSED_EVENT, REOPENED_EVENT, MERGED_EVENT,
        HEAD_REF_FORCE_PUSHED_EVENT, PULL_REQUEST_COMMIT]) {
      pageInfo { hasNextPage endCursor }
      nodes { __typename
        ... on ReadyForReviewEvent { createdAt }
        ... on ConvertToDraftEvent { createdAt }
        ... on ClosedEvent { createdAt }
        ... on ReopenedEvent { createdAt }
        ... on MergedEvent { createdAt }
        ... on HeadRefForcePushedEvent { createdAt beforeCommit { oid } afterCommit { oid } }
        ... on PullRequestCommit { commit { oid } }
      }
    }
  } }
}"""
COMMIT_FIELDS = """... on Commit {
    parents(first: 5) { nodes { oid } } checkSuites(first: 1) { totalCount } status { contexts { context } }
  }"""
SHA = re.compile(r"\b[0-9a-f]{40}\b")
EDITS_QUERY = """query($id: ID!, $cursor: String) {
  node(id: $id) { ... on IssueComment {
    userContentEdits(first: 100, after: $cursor) {
      pageInfo { hasNextPage endCursor } nodes { editedAt diff }
    }
  } }
}"""


def gh(*args: str) -> Any:
    for attempt in range(6):
        completed = subprocess.run(["gh", *args], capture_output=True, text=True)
        if completed.returncode == 0:
            return json.loads(completed.stdout) if completed.stdout.strip() else None
        error = completed.stderr
        if "rate limit" in error.lower() or "secondary" in error.lower():
            time.sleep(60)
            continue
        if attempt < 5 and ("502" in error or "504" in error or "timeout" in error.lower()):
            time.sleep(5 * (attempt + 1))
            continue
        raise RuntimeError(f"gh {' '.join(args[:3])}: {error.strip()[:300]}")
    raise RuntimeError(f"gh {' '.join(args[:3])}: still rate limited")


def graphql(query: str, **variables: Any) -> dict[str, Any]:
    args = ["api", "graphql", "-f", f"query={query}"]
    for key, value in variables.items():
        if value is not None:
            args += ["-F" if isinstance(value, int) else "-f", f"{key}={value}"]
    data = gh(*args)
    if not isinstance(data, dict) or data.get("errors"):
        raise RuntimeError(f"GraphQL error: {json.dumps((data or {}).get('errors'))[:300]}")
    return data["data"]


def rest_items(endpoint: str) -> list[dict[str, Any]]:
    pages = gh("api", "--paginate", "--slurp", endpoint)
    return [item for page in pages for item in page]


def require_public(repo: str) -> None:
    """Refuse a repository an anonymous request cannot read as public."""
    request = urllib.request.Request(
        f"https://api.github.com/repos/{repo}",
        headers={"Accept": "application/vnd.github+json", "User-Agent": "kgr-coderabbit-recorder"},
    )
    try:
        with urllib.request.urlopen(request, timeout=30) as response:
            value = json.load(response)
    except urllib.error.HTTPError as error:
        raise SystemExit(f"refusing {repo}: anonymous read returned HTTP {error.code}") from None
    if value.get("private") is not False or value.get("visibility") != "public":
        raise SystemExit(f"refusing {repo}: it is not public")


def reduce_body(body: str) -> str:
    """The lines of a CodeRabbit body the adapter reads; all other prose is dropped."""
    kept = []
    for line in (body or "").splitlines():
        if (
            line.strip() == ADAPTER.SUMMARY_MARKER
            or BLOCK_MARKER.match(line)
            or ADAPTER.COVERAGE_MARKER.search(line)
            or WAIT_SENTENCE.search(line)
            or ADAPTER.NO_ACTIONABLE in line
            or ADAPTER.ACTIONABLE.search(line)
            or ADAPTER.REVIEW_FINDINGS.search(line)
        ):
            kept.append(line.strip())
    return "\n".join(kept)


def reduce_command(body: str) -> str | None:
    """A non-CodeRabbit comment that mentions CodeRabbit, reduced line by line; None if none does."""
    lines = (body or "").strip().splitlines()
    if not any(COMMAND.search(line) for line in lines):
        return None
    kept = []
    for line in lines:
        text = " ".join(line.split())
        match = COMMAND.search(text)
        if not match:
            kept.append("[text]" if text else "")
            continue
        command = " ".join(match.group(0).split()).lower()
        before = "[text] " if text[: match.start()].strip() else ""
        after = " [text]" if text[match.end():].strip() else ""
        kept.append(f"{before}{command}{after}")
    return "\n".join(kept)


def is_bot(login: str) -> bool:
    return login.lower() in ADAPTER.BOT_LOGINS


def comment_versions(item: dict[str, Any], shas: set[str]) -> tuple[list[list[str]], bool]:
    """Every reduced version of a CodeRabbit comment; adds the commits its bodies name to `shas`."""
    created, updated = item["created_at"], item["updated_at"]
    shas.update(SHA.findall(item["body"] or ""))
    if updated <= created:
        return [[created, reduce_body(item["body"])]], True
    nodes: list[dict[str, Any]] = []
    cursor = None
    while True:
        data = graphql(EDITS_QUERY, id=item["node_id"], cursor=cursor)
        edits = ((data.get("node") or {}).get("userContentEdits")) or {}
        nodes += [node for node in edits.get("nodes") or [] if node]
        shas.update(sha for node in nodes for sha in SHA.findall(node.get("diff") or ""))
        if not (edits.get("pageInfo") or {}).get("hasNextPage"):
            break
        cursor = edits["pageInfo"]["endCursor"]
    versions = sorted(
        [[node["editedAt"], reduce_body(node.get("diff") or "")] for node in nodes if node.get("diff") is not None]
    )
    complete = bool(versions) and len(versions) == len(nodes)
    if not versions or versions[-1][0] < updated:
        versions.append([updated, reduce_body(item["body"])])
    return versions, complete


def record(repo: str, number: int) -> dict[str, Any]:
    owner, name = repo.split("/", 1)
    items: list[dict[str, Any]] = []
    cursor = None
    while True:
        pull = graphql(PULL_QUERY, owner=owner, name=name, number=number, cursor=cursor)["repository"]["pullRequest"]
        items += pull["timelineItems"]["nodes"]
        if not pull["timelineItems"]["pageInfo"]["hasNextPage"]:
            break
        cursor = pull["timelineItems"]["pageInfo"]["endCursor"]
    kinds = {
        "ReadyForReviewEvent": "ready",
        "ConvertToDraftEvent": "draft",
        "ClosedEvent": "closed",
        "ReopenedEvent": "reopened",
        "MergedEvent": "merged",
    }
    events = sorted(
        ({"type": kinds[item["__typename"]], "at": item["createdAt"]} for item in items if item["__typename"] in kinds),
        key=lambda event: event["at"],
    )
    shas = {pull["headRefOid"]}
    for item in items:
        if item["__typename"] == "PullRequestCommit":
            shas.add(item["commit"]["oid"])
        elif item["__typename"] == "HeadRefForcePushedEvent":
            for key in ("beforeCommit", "afterCommit"):
                if item.get(key):
                    shas.add(item[key]["oid"])
    comments = []
    for item in rest_items(f"repos/{repo}/issues/{number}/comments?per_page=100"):
        login = item["user"]["login"]
        if is_bot(login):
            # A force push drops earlier heads from the timeline; CodeRabbit's comments still name them.
            versions, complete = comment_versions(item, shas)
            comments.append(
                {"id": item["id"], "user": {"login": login}, "created_at": item["created_at"],
                 "versions": versions, "historyComplete": complete}
            )
        else:
            command = reduce_command(item["body"])
            if command is not None:
                comments.append(
                    {"id": item["id"], "user": {"login": "maintainer"}, "created_at": item["created_at"],
                     "versions": [[item["created_at"], command]], "historyComplete": True}
                )
    reviews = []
    for item in rest_items(f"repos/{repo}/pulls/{number}/reviews?per_page=100"):
        if is_bot(item["user"]["login"]) and item.get("submitted_at"):
            shas.add(item["commit_id"])
            shas.update(SHA.findall(item.get("body") or ""))
            reviews.append(
                {"id": item["id"], "user": {"login": item["user"]["login"]}, "commit_id": item["commit_id"],
                 "submitted_at": item["submitted_at"], "body": reduce_body(item.get("body") or "")}
            )
    heads: dict[str, Any] = {}
    ordered = sorted(shas)
    for offset in range(0, len(ordered), 40):
        batch = ordered[offset:offset + 40]
        fields = " ".join(f'c{index}: object(oid: "{sha}") {{ {COMMIT_FIELDS} }}' for index, sha in enumerate(batch))
        query = f"query($owner: String!, $name: String!) {{ repository(owner: $owner, name: $name) {{ {fields} }} }}"
        found = graphql(query, owner=owner, name=name)["repository"]
        for index, sha in enumerate(batch):
            commit = found.get(f"c{index}")
            if not commit or "parents" not in commit:
                continue
            suites = []
            if commit["checkSuites"]["totalCount"]:
                suites = [
                    {"created_at": suite["created_at"], "head_branch": suite["head_branch"]}
                    for page in gh("api", "--paginate", "--slurp", f"repos/{repo}/commits/{sha}/check-suites?per_page=100")
                    for suite in page["check_suites"]
                ]
            # Only a commit pushed to this branch, or its final head, was a head of the pull request.
            if sha != pull["headRefOid"] and not any(suite["head_branch"] == pull["headRefName"] for suite in suites):
                continue
            contexts = [context["context"] for context in ((commit.get("status") or {}).get("contexts") or [])]
            statuses = []
            if any("coderabbit" in context.lower() for context in contexts):
                statuses = [
                    {
                        "id": status["id"],
                        "state": status["state"],
                        "description": status["description"],
                        "context": status["context"],
                        "created_at": status["created_at"],
                        "creator": {"login": status["creator"]["login"]},
                    }
                    for status in rest_items(f"repos/{repo}/commits/{sha}/statuses?per_page=100")
                    if ADAPTER.is_coderabbit(status)
                ]
            heads[sha] = {
                "parents": [parent["oid"] for parent in commit["parents"]["nodes"]],
                "checkSuites": sorted(suites, key=lambda suite: suite["created_at"]),
                "statuses": sorted(statuses, key=lambda status: (status["created_at"], status["id"])),
            }
    initial_draft = next(
        (event["type"] == "ready" for event in events if event["type"] in {"ready", "draft"}),
        bool(pull["isDraft"]),
    )
    return {
        "repo": repo,
        "number": number,
        "createdAt": pull["createdAt"],
        "ref": pull["headRefName"],
        "base": pull["baseRefName"],
        "head": pull["headRefOid"],
        "state": pull["state"],
        "initialDraft": initial_draft,
        "events": events,
        "heads": heads,
        "comments": sorted(comments, key=lambda comment: (comment["created_at"], comment["id"])),
        "reviews": sorted(reviews, key=lambda review: (review["submitted_at"], review["id"])),
    }


def coderabbit_activity(pull: dict[str, Any]) -> list[str]:
    """When CodeRabbit posted a status, comment version or review on the pull request."""
    times = [status["created_at"] for head in pull["heads"].values() for status in head["statuses"]]
    times += [version[0] for item in pull["comments"] if is_bot(item["user"]["login"]) for version in item["versions"]]
    times += [review["submitted_at"] for review in pull["reviews"]]
    return sorted(times)


def in_window(pull: dict[str, Any], since: str) -> bool:
    activity = coderabbit_activity(pull)
    return bool(activity) and activity[0] >= since


def parse_refs(values: list[str]) -> list[tuple[str, int]]:
    refs = []
    for value in values:
        if value.startswith("@"):
            refs += parse_refs([line.strip() for line in Path(value[1:]).read_text().splitlines() if line.strip()])
            continue
        repo, separator, number = value.partition("#")
        if not separator or repo.count("/") != 1 or not number.isdigit():
            raise SystemExit(f"not OWNER/REPO#N: {value}")
        refs.append((repo, int(number)))
    return list(dict.fromkeys(refs))


def save(path: Path, corpus: dict[str, Any]) -> None:
    corpus["pullRequests"].sort(key=lambda pull: (pull["repo"], pull["number"]))
    lines = ",\n".join(json.dumps(pull, separators=(",", ":"), sort_keys=True) for pull in corpus["pullRequests"])
    header = json.dumps({key: value for key, value in corpus.items() if key != "pullRequests"}, indent=1)
    path.write_text(header[:-2] + ',\n "pullRequests": [\n' + lines + "\n]\n}\n")


def main() -> int:
    arguments = argparse.ArgumentParser(description=__doc__.split("\n", 1)[0])
    arguments.add_argument("--out", required=True, type=Path)
    arguments.add_argument("--since", required=True, help="YYYY-MM-DD; older CodeRabbit activity is dropped")
    arguments.add_argument("--workers", type=int, default=4)
    arguments.add_argument("refs", nargs="*", help="OWNER/REPO#N, or @FILE with one per line")
    args = arguments.parse_args()
    refs = parse_refs(args.refs)
    for repo in sorted({repo for repo, _number in refs}):
        require_public(repo)
    corpus = (
        json.loads(args.out.read_text())
        if args.out.exists()
        else {"pullRequests": []}
    )
    corpus["about"] = ABOUT
    corpus["since"] = args.since
    corpus["pullRequests"] = [pull for pull in corpus["pullRequests"] if in_window(pull, args.since)]
    for repo in {pull["repo"] for pull in corpus["pullRequests"]}:
        require_public(repo)
    done = {(pull["repo"], pull["number"]) for pull in corpus["pullRequests"]}
    todo = [ref for ref in refs if ref not in done]
    with ThreadPoolExecutor(args.workers) as pool:
        for index, pull in enumerate(pool.map(lambda ref: record(*ref), todo), 1):
            kept = in_window(pull, args.since)
            if kept:
                corpus["pullRequests"].append(pull)
            print(f"{index}/{len(todo)} {pull['repo']}#{pull['number']} heads={len(pull['heads'])}"
                  f"{'' if kept else ' dropped: CodeRabbit activity before --since'}", flush=True)
            if index % 20 == 0:
                save(args.out, corpus)
    if todo:
        corpus["recordedAt"] = time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())
    save(args.out, corpus)
    return 0


if __name__ == "__main__":
    sys.exit(main())
