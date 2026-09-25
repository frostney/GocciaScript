"""Scope-bound, independently verified GitHub feedback mutations."""
from pathlib import Path
from typing import Any

from kgr_github import StateLock, WaitError, load_state, stable_digest, write_state

HEAD_QUERY = "query($owner:String!,$name:String!,$number:Int!){repository(owner:$owner,name:$name){pullRequest(number:$number){headRefOid}}}"
THREAD_QUERY = "query($thread:ID!){node(id:$thread){... on PullRequestReviewThread{id isResolved pullRequest{number headRefOid repository{nameWithOwner}}}}}"
RESOLVE_MUTATION = "mutation($thread:ID!){resolveReviewThread(input:{threadId:$thread}){thread{id isResolved}}}"


def result(state: str, observation: dict[str, Any], reason: str) -> dict[str, Any]:
    return {"state": state, "observation": observation, "reason": reason}


def observed_head(gh, repo: str, pr: int) -> str:
    owner, name = repo.split("/", 1)
    value = gh.graphql(HEAD_QUERY, {"owner": owner, "name": name, "number": pr})
    head = ((value.get("repository") or {}).get("pullRequest") or {}).get("headRefOid")
    if not isinstance(head, str) or not head:
        raise WaitError("cannot verify pull request head")
    return head


def comment_scope(value: dict[str, Any], repo: str, pr: int) -> bool:
    # REST links are authoritative API URLs, including enterprise host prefixes.
    from urllib.parse import urlsplit
    path = urlsplit(str(value.get("pull_request_url") or "")).path
    return path.lower().endswith(f"/repos/{repo}/pulls/{pr}".lower())


def reply(gh, repo: str, pr: int, head: str, comment_id: int, body: str,
          operation_id: str, state_path: Path) -> dict[str, Any]:
    if not operation_id.replace("-", "").replace("_", "").replace(".", "").replace(":", "").isalnum():
        raise WaitError("--operation-id may contain letters, digits, dot, colon, dash, and underscore")
    with StateLock(state_path):
        current = observed_head(gh, repo, pr)
        if current != head:
            return result("invalidated", {"head": current}, "pull request head changed before reply")
        actor = (gh.rest("user") or {}).get("login")
        if not isinstance(actor, str) or not actor:
            raise WaitError("cannot verify authenticated reply author")
        target = gh.rest(f"repos/{repo}/pulls/comments/{comment_id}")
        if target.get("id") != comment_id or not comment_scope(target, repo, pr):
            raise WaitError("reply comment does not belong to the requested pull request")
        root_id = target.get("in_reply_to_id") or comment_id
        if root_id != comment_id:
            root = gh.rest(f"repos/{repo}/pulls/comments/{root_id}")
            if root.get("id") != root_id or not comment_scope(root, repo, pr) or root.get("in_reply_to_id"):
                raise WaitError("cannot verify reply thread root")
        request = {"repo": repo, "pr": pr, "head": head, "commentId": comment_id,
                   "rootId": root_id, "bodyDigest": stable_digest(body), "actor": actor.lower(), "operationId": operation_id}
        request_digest = stable_digest(request)
        marker = f"<!-- known-good-route-operation:{operation_id} -->"
        full_body = f"{body}\n\n{marker}\n<!-- known-good-route-request:{request_digest} -->"
        prior = load_state(state_path)
        if prior and (prior.get("kind") != "review-reply" or prior.get("request") != request):
            raise WaitError("reply checkpoint belongs to a different request; existing operation cannot be reused")

        def verify(item):
            if (not comment_scope(item, repo, pr) or item.get("in_reply_to_id") != root_id or
                    str((item.get("user") or {}).get("login") or "").lower() != actor.lower() or item.get("body") != full_body):
                raise WaitError("reply receipt does not match the requested target, author and body")
            return item

        def discover():
            pages = gh.rest_pages(f"repos/{repo}/pulls/{pr}/comments?per_page=100")
            if not all(isinstance(page, list) for page in pages):
                raise WaitError("cannot verify complete reply discovery")
            matches = [item for page in pages for item in page if marker in str(item.get("body") or "")]
            if len(matches) > 1:
                raise WaitError("multiple replies share this operation marker")
            if not matches:
                return None
            identifier = verify(matches[0]).get("id")
            if not isinstance(identifier, int) or identifier <= 0:
                raise WaitError("reply receipt has no valid comment identity")
            fetched = gh.rest(f"repos/{repo}/pulls/comments/{identifier}")
            if fetched.get("id") != identifier:
                raise WaitError("reply lookup returned a different comment identity")
            return verify(fetched)

        receipt = discover()
        created = False
        if receipt is None and prior:
            return result("pending", {"operationId": operation_id, "requestDigest": request_digest},
                          "prior reply write is unconfirmed; reconcile without posting another reply")
        if receipt is None:
            current = observed_head(gh, repo, pr)
            if current != head:
                return result("invalidated", {"head": current}, "pull request head changed before reply write")
            if str((gh.rest("user") or {}).get("login") or "").lower() != actor.lower():
                raise WaitError("authenticated reply author changed before write")
            checkpoint = {"schemaVersion": 1, "kind": "review-reply", "request": request, "phase": "posting"}
            write_state(state_path, checkpoint)
            try:
                posted = gh.rest(f"repos/{repo}/pulls/{pr}/comments/{root_id}/replies", "POST", {"body": full_body})
                checkpoint["commentId"] = posted.get("id")
                write_state(state_path, checkpoint)
                created = True
            except WaitError as error:
                created = None
                checkpoint["writeError"] = str(error)
                write_state(state_path, checkpoint)
            try:
                receipt = discover()
            except WaitError as error:
                checkpoint["verificationError"] = str(error)
                write_state(state_path, checkpoint)
                return result("pending", {"operationId": operation_id, "requestDigest": request_digest},
                              "reply write requires independent reconciliation: " + str(error))
            if receipt is None:
                return result("pending", {"operationId": operation_id, "requestDigest": request_digest},
                              "reply write has no independently verified receipt; do not repost")
        write_state(state_path, {"schemaVersion": 1, "kind": "review-reply", "request": request,
                                "phase": "confirmed", "commentId": receipt["id"]})
        try:
            current = observed_head(gh, repo, pr)
        except WaitError as error:
            return result("pending", {"commentId": receipt["id"], "rootId": root_id,
                          "operationId": operation_id, "created": created},
                          "reply verified but current head is unavailable: " + str(error))
        observation = {"commentId": receipt["id"], "rootId": root_id, "operationId": operation_id,
                       "requestDigest": request_digest, "created": created, "head": current, "author": actor}
        if current != head:
            return result("invalidated", observation, "reply exists but the pull request head changed; readiness requires fresh evidence")
        return result("satisfied", observation, "matching inline reply independently verified")


def resolve(gh, repo: str, pr: int, head: str, thread_id: str) -> dict[str, Any]:
    def read():
        value = gh.graphql(THREAD_QUERY, {"thread": thread_id}).get("node")
        if not isinstance(value, dict) or value.get("id") != thread_id:
            raise WaitError("cannot verify review thread identity")
        pull = value.get("pullRequest") or {}
        if pull.get("number") != pr or str((pull.get("repository") or {}).get("nameWithOwner") or "").lower() != repo.lower():
            raise WaitError("review thread does not belong to the requested repository and pull request")
        if type(value.get("isResolved")) is not bool or not pull.get("headRefOid"):
            raise WaitError("cannot verify current thread/head state")
        return value, pull["headRefOid"]
    thread, current = read()
    if current != head:
        return result("invalidated", {"head": current, "threadId": thread_id}, "thread's pull request head changed before resolution")
    attempted = not thread["isResolved"]
    mutation_error = None
    if attempted:
        try:
            gh.graphql(RESOLVE_MUTATION, {"thread": thread_id})
        except WaitError as error:
            mutation_error = str(error)
    try:
        thread, current = read()
    except WaitError as error:
        return result("pending", {"threadId": thread_id, "mutationAttempted": attempted},
                      "resolution requires independent reconciliation: " + str(error))
    observation = {"threadId": thread_id, "resolved": thread["isResolved"], "head": current,
                   "mutationAttempted": attempted, "mutationError": mutation_error}
    if current != head:
        return result("invalidated", observation, "thread state observed but head changed during resolution; readiness requires fresh evidence")
    if not thread["isResolved"]:
        return result("pending", observation, "thread resolution is not independently confirmed")
    return result("satisfied", observation, "current scoped thread resolution independently verified")
