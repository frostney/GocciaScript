"""Paginated GitHub review facts and temporal evidence selection."""
from copy import deepcopy
from typing import Any

from kgr_github import TransientError, WaitError, parse_time, stable_digest

PAGE_INFO = "pageInfo{hasNextPage endCursor} totalCount"
COMMENT_FIELDS = "id databaseId body createdAt updatedAt author{login} authorAssociation"
REVIEW_COMMENT_FIELDS = "id replyTo{id}"
REVIEW_FIELDS = "id databaseId author{login} authorAssociation state body createdAt updatedAt submittedAt commit{oid} comments(first:100){nodes{" + REVIEW_COMMENT_FIELDS + "} " + PAGE_INFO + "}"
THREAD_COMMENT_FIELDS = COMMENT_FIELDS + " replyTo{id}"
THREAD_FIELDS = "id isResolved comments(first:100){nodes{" + THREAD_COMMENT_FIELDS + "} " + PAGE_INFO + "}"
CHECK_FIELDS = """__typename
... on CheckRun{id name status conclusion startedAt completedAt checkSuite{app{slug}}}
... on StatusContext{id context state createdAt creator{login}}
"""
FIELDS = {"comments": COMMENT_FIELDS, "reviews": REVIEW_FIELDS,
          "reviewThreads": THREAD_FIELDS, "contexts": CHECK_FIELDS}


def connection(field: str, after: bool = False) -> str:
    return field + "(first:100" + (",after:$after" if after else "") + "){nodes{" + FIELDS[field] + "} " + PAGE_INFO + "}"


def commit_contexts(value: str) -> str:
    return "commits(last:1){nodes{commit{statusCheckRollup{" + value + "}}}}"


def pull_query(body: str, after: bool = False) -> str:
    return "query($owner:String!,$name:String!,$number:Int!" + (",$after:String!" if after else "") + "){repository(owner:$owner,name:$name){pullRequest(number:$number){headRefOid " + body + "}}}"


REVIEW_QUERY = pull_query(" ".join(connection(k) for k in ("comments", "reviews", "reviewThreads")) + " " + commit_contexts(connection("contexts")))


def extract_pull(data: dict[str, Any]) -> dict[str, Any]:
    pull = (data.get("repository") or {}).get("pullRequest")
    if not isinstance(pull, dict) or not pull.get("headRefOid"):
        raise WaitError("pull request has no verified head")
    return pull


def contexts(pull: dict[str, Any]) -> dict[str, Any]:
    commits = (pull.get("commits") or {}).get("nodes") or []
    if len(commits) != 1:
        raise WaitError("pull request has no verified head commit")
    rollup = (commits[0].get("commit") or {}).get("statusCheckRollup")
    if rollup is None:
        return {"nodes": [], "pageInfo": {"hasNextPage": False}, "totalCount": 0}
    return rollup.get("contexts")


def collect(first: dict[str, Any], fetch, label: str) -> list[dict[str, Any]]:
    """Follow every cursor; never convert partial or racing data to absence."""
    page = first
    nodes = []
    cursors: set[str] = set()
    ids: set[str] = set()
    total = first.get("totalCount") if isinstance(first, dict) else None
    if type(total) is not int or total < 0:
        raise WaitError(f"missing {label} total count")
    while True:
        if not isinstance(page, dict) or not isinstance(page.get("nodes"), list):
            raise WaitError(f"missing {label} connection")
        info = page.get("pageInfo")
        if not isinstance(info, dict) or type(info.get("hasNextPage")) is not bool:
            raise WaitError(f"missing {label} pagination state")
        if page.get("totalCount") != total:
            raise TransientError(f"{label} count changed during pagination")
        for node in page["nodes"]:
            if not isinstance(node, dict):
                raise WaitError(f"inaccessible {label} node")
            identity = node.get("id")
            if not isinstance(identity, str) or not identity:
                raise WaitError(f"missing {label} node identity")
            if identity and identity in ids:
                raise TransientError(f"duplicate {label} node across pages")
            if identity:
                ids.add(identity)
            nodes.append(node)
        if not info["hasNextPage"]:
            break
        cursor = info.get("endCursor")
        if not isinstance(cursor, str) or not cursor or cursor in cursors:
            raise WaitError(f"non-advancing {label} pagination cursor")
        cursors.add(cursor)
        page = fetch(cursor)
    if len(nodes) != total:
        raise TransientError(f"incomplete {label} census")
    return sorted(nodes, key=lambda item: str(item.get("id") or stable_digest(item)))


def read_census(gh, variables: dict[str, Any], page_size: int) -> dict[str, Any]:
    sized = lambda query: query.replace("first:100", f"first:{page_size}")
    pull = deepcopy(extract_pull(gh.graphql(sized(REVIEW_QUERY), variables)))
    head = pull["headRefOid"]

    def fetch(field: str, after: str):
        body = connection(field, True)
        if field == "contexts":
            body = commit_contexts(body)
        current = extract_pull(gh.graphql(sized(pull_query(body, True)), variables | {"after": after}))
        if current["headRefOid"] != head:
            raise TransientError("pull request head changed during review pagination")
        return contexts(current) if field == "contexts" else current.get(field)

    result = {"headRefOid": head}
    for field in ("comments", "reviews", "reviewThreads", "contexts"):
        first = contexts(pull) if field == "contexts" else pull.get(field)
        result[field] = collect(first, lambda cursor, field=field: fetch(field, cursor), field)
    for thread in result["reviewThreads"]:
        def fetch_comments(after: str):
            query = "query($thread:ID!,$after:String!){node(id:$thread){... on PullRequestReviewThread{id isResolved pullRequest{headRefOid} comments(first:100,after:$after){nodes{" + THREAD_COMMENT_FIELDS + "} " + PAGE_INFO + "}}}}"
            current = gh.graphql(sized(query), {"thread": thread["id"], "after": after}).get("node")
            if not isinstance(current, dict) or current.get("id") != thread["id"]:
                raise TransientError("review thread disappeared during pagination")
            if ((current.get("pullRequest") or {}).get("headRefOid") != head or
                    current.get("isResolved") != thread.get("isResolved")):
                raise TransientError("review thread/head changed during pagination")
            return current.get("comments")
        thread["comments"] = collect(thread.get("comments"), fetch_comments, f"thread {thread.get('id')} comments")
    for review in result["reviews"]:
        def fetch_review_comments(after: str):
            query = "query($review:ID!,$after:String!){node(id:$review){... on PullRequestReview{id pullRequest{headRefOid} comments(first:100,after:$after){nodes{" + REVIEW_COMMENT_FIELDS + "} " + PAGE_INFO + "}}}}"
            current = gh.graphql(sized(query), {"review": review["id"], "after": after}).get("node")
            if not isinstance(current, dict) or current.get("id") != review["id"] or (current.get("pullRequest") or {}).get("headRefOid") != head:
                raise TransientError("review/head changed during comment pagination")
            return current.get("comments")
        review["comments"] = collect(review.get("comments"), fetch_review_comments, f"review {review.get('id')} comments")
    return result


def review_census(gh, variables: dict[str, Any], page_size: int = 100) -> dict[str, Any]:
    if type(page_size) is not int or not 1 <= page_size <= 100:
        raise WaitError("review page size must be between 1 and 100")
    first = read_census(gh, variables, page_size)
    second = read_census(gh, variables, page_size)
    if first != second:
        raise TransientError("review census changed during observation; fresh evidence is required")
    return second


def instant(*values: Any) -> float | None:
    supplied = [value for value in values if value is not None]
    if not supplied:
        return None
    try:
        return max(parse_time(value) for value in supplied if isinstance(value, str)) if all(isinstance(v, str) for v in supplied) else None
    except WaitError:
        return None


def terminal_evidence(checks: list[dict[str, Any]], reviews: list[dict[str, Any]], automation: dict[str, Any]) -> dict[str, Any]:
    conclusions = {value.lower() for value in automation.get("terminal_check_conclusions", [])}
    states = {value.upper() for value in automation.get("terminal_review_states", [])}
    markers = [value.lower() for value in automation.get("nonterminal_review_markers", [])]
    groups: dict[tuple, list[dict[str, Any]]] = {}
    for item in checks:
        event = {"kind": "check", "id": item.get("id"), "attemptAt": instant(item.get("startedAt")),
                 "at": instant(item.get("completedAt"), item.get("startedAt")),
                 "accepted": item["status"] == "COMPLETED" and item["conclusion"] in conclusions,
                 "active": item["status"] != "COMPLETED"}
        groups.setdefault(("check", item.get("source"), item["name"], item["app"]), []).append(event)
    for item in reviews:
        at = instant(item.get("createdAt"), item.get("submittedAt"), item.get("updatedAt"))
        state = str(item.get("state") or "").upper()
        comments = item.get("comments", [])
        if state == "COMMENTED" and not str(item.get("body") or "").strip() and comments and all(comment.get("replyTo") for comment in comments):
            # GitHub creates an empty COMMENTED review for an inline reply.
            # This transport record is not a new automation verdict.
            continue
        event = {"kind": "review", "id": item.get("id"), "attemptAt": at, "at": at,
                 "accepted": state in states and not any(marker in str(item.get("body") or "").lower() for marker in markers),
                 "active": state == "PENDING"}
        groups.setdefault(("review", str((item.get("author") or {}).get("login") or "").lower()), []).append(event)
    latest = []
    ambiguous = False
    for events in groups.values():
        if any(item["attemptAt"] is None for item in events):
            selected = events
        else:
            newest = max(item["attemptAt"] for item in events)
            selected = [item for item in events if item["attemptAt"] == newest]
        if any(item["accepted"] for item in selected) and any(not item["accepted"] for item in selected):
            ambiguous = True
        latest.extend(selected)
    accepted = [item for item in latest if item["accepted"]]
    pending = [item for item in latest if not item["accepted"]]
    terminal = bool(accepted) and not ambiguous and not any(item["active"] for item in pending)
    if terminal and pending:
        known_successes = [item["at"] for item in accepted if item["at"] is not None]
        terminal = bool(known_successes) and all(item["at"] is not None and item["at"] < max(known_successes) for item in pending)
    return {"terminal": terminal, "latestEvidence": latest, "ambiguousOrder": ambiguous,
            "terminalReason": "current terminal evidence has no newer incomplete attempt" if terminal else "missing, incomplete or ambiguously ordered review evidence"}
