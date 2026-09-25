"""Complete and current review evidence, independent of model wording."""
import importlib.util
import sys
import unittest
from pathlib import Path
from copy import deepcopy
import re

SCRIPTS = Path(__file__).resolve().parents[1] / "scripts"
sys.path.insert(0, str(SCRIPTS))
spec = importlib.util.spec_from_file_location("review_wait", SCRIPTS / "review_wait.py")
review = importlib.util.module_from_spec(spec)
spec.loader.exec_module(review)


class StaticGH:
    def __init__(self, checks=(), reviews=()):
        self.pull = {
            "headRefOid": "head",
            "comments": {"nodes": [], "pageInfo": {"hasNextPage": False}},
            "reviewThreads": {"nodes": [], "pageInfo": {"hasNextPage": False}},
            "reviews": {"nodes": list(reviews), "pageInfo": {"hasNextPage": False}},
            "commits": {"nodes": [{"commit": {"statusCheckRollup": {"contexts": {
                "nodes": list(checks), "pageInfo": {"hasNextPage": False},
            }}}}]},
        }

    def graphql(self, query, variables):
        value = deepcopy(self.pull)
        def counts(item):
            if isinstance(item, dict):
                if "nodes" in item and "pageInfo" in item:
                    item["totalCount"] = len(item["nodes"])
                for child in item.values():
                    counts(child)
            elif isinstance(item, list):
                for child in item:
                    counts(child)
        counts(value)
        return {"repository": {"pullRequest": value}}


def check(identity, start, end=None, status="COMPLETED", conclusion="SUCCESS"):
    return {"__typename": "CheckRun", "id": identity, "name": "Review", "startedAt": start,
            "completedAt": end, "status": status, "conclusion": conclusion,
            "checkSuite": {"app": {"slug": "review-app"}}}


def verdict(identity, at, body="", state="APPROVED"):
    return {"id": identity, "author": {"login": "reviewer[bot]"}, "commit": {"oid": "head"},
            "createdAt": at, "updatedAt": at, "submittedAt": at if state != "PENDING" else None,
            "body": body, "state": state, "comments": {"nodes": [], "totalCount": 0, "pageInfo": {"hasNextPage": False}}}


POLICY = {"automations": [{"id": "reviewer", "actors": ["reviewer[bot]"],
    "check_contexts": ["Review"], "check_app_slugs": ["review-app"],
    "terminal_check_conclusions": ["success"], "terminal_review_states": ["APPROVED", "COMMENTED"],
    "nonterminal_review_markers": ["rate limit"]}]}
T1, T2, T3 = (f"2026-09-06T01:0{i}:00Z" for i in range(1, 4))


class ReviewCurrencyTest(unittest.TestCase):
    def terminal(self, checks=(), reviews=()):
        return review.review_snapshot(StaticGH(checks, reviews), "owner/repo", 1, POLICY)["automations"][0]["terminal"]

    def test_new_check_attempt_invalidates_prior_success_regardless_of_array_order(self):
        old = check("old", T1, T2)
        new = check("new", T3, status="IN_PROGRESS", conclusion=None)
        for nodes in ([old, new], [new, old]):
            self.assertFalse(self.terminal(nodes))

    def test_new_failed_attempt_invalidates_prior_success(self):
        self.assertFalse(self.terminal([check("old", T1, T1), check("new", T2, T3, conclusion="FAILURE")]))

    def test_new_success_replaces_failed_attempt(self):
        self.assertTrue(self.terminal([check("old", T1, T1, conclusion="FAILURE"), check("new", T2, T3)]))

    def test_new_incomplete_review_blocks_an_older_successful_check(self):
        self.assertFalse(self.terminal([check("old", T1, T1)], [verdict("new", T3, "rate limit", "COMMENTED")]))

    def test_latest_review_state_replaces_old_approval(self):
        for state, body in (("PENDING", ""), ("COMMENTED", "rate limit"), ("DISMISSED", "")):
            with self.subTest(state=state):
                self.assertFalse(self.terminal(reviews=[verdict("old", T1), verdict("new", T3, body, state)]))

    def test_new_completion_can_supersede_an_older_rate_limit_notice(self):
        self.assertTrue(self.terminal([check("new", T2, T3)], [verdict("old", T1, "rate limit", "COMMENTED")]))

    def test_unknown_or_tied_conflicting_order_never_proves_completion(self):
        for at in (None, T1):
            with self.subTest(at=at):
                self.assertFalse(self.terminal(reviews=[verdict("yes", T1), verdict("no", at, "rate limit", "COMMENTED")]))

    def test_timezone_offsets_are_compared_as_instants(self):
        self.assertFalse(self.terminal(reviews=[verdict("yes", "2026-09-06T02:00:00+02:00"), verdict("no", "2026-09-06T01:00:00Z", "rate limit", "COMMENTED")]))

    def test_tied_attempt_start_cannot_be_ordered_by_completion_time(self):
        self.assertFalse(self.terminal([check("yes", T1, T3), check("no", T1, T2, conclusion="FAILURE")]))

    def test_pending_status_context_blocks_even_a_later_review(self):
        pending = {"__typename": "StatusContext", "id": "status", "context": "Review",
                   "state": "PENDING", "createdAt": T1, "creator": {"login": "review-app"}}
        self.assertFalse(self.terminal([pending], [verdict("later", T3)]))

    def test_reply_only_review_cannot_supersede_incomplete_verdict(self):
        reply = verdict("reply", T3, state="COMMENTED")
        reply["comments"]["nodes"] = [{"id": "reply-comment", "replyTo": {"id": "original"}}]
        self.assertFalse(self.terminal([check("old", T1, T1)], [verdict("pending", T2, "rate limit", "COMMENTED"), reply]))

    def test_inline_review_without_summary_can_still_supply_a_verdict(self):
        inline = verdict("inline", T3, state="COMMENTED")
        inline["comments"]["nodes"] = [{"id": "finding", "replyTo": None}]
        self.assertTrue(self.terminal(reviews=[verdict("pending", T2, "rate limit", "COMMENTED"), inline]))


class PagedGH:
    """Cursor transport fixture with late findings on every independent connection."""
    def __init__(self):
        self.calls = []
        self.head = "head"
        self.collections = {
            "comments": [{"id": f"comment-{i}", "body": "late-comment" if i == 200 else "",
                          "author": {"login": "human"}} for i in range(201)],
            "reviews": [verdict(f"review-{i}", T2, "late-review" if i == 200 else "") for i in range(201)],
            "contexts": [check(f"check-{i}", T1, T2) for i in range(101)],
            "reviewThreads": [{"id": f"thread-{i}", "isResolved": i != 100} for i in range(101)],
        }
        for i, item in enumerate(self.collections["reviews"]):
            item["author"]["login"] = "human"
            if i != 200:
                item["commit"]["oid"] = "old-head"
        for i, item in enumerate(self.collections["contexts"]):
            if i != 100:
                item["name"] = f"unrelated-{i}"
        self.thread_comments = [{"id": f"nested-{i}", "body": "finding" if i == 0 else "",
            "createdAt": T3 if i == 200 else T1,
            "author": {"login": "reviewer[bot]" if i == 0 else "human"},
            "authorAssociation": "OWNER" if i == 200 else "NONE"} for i in range(201)]
        self.review_comments = {}
        self.mutate = lambda query, variables, result: None

    def page(self, field, after=None):
        start = int(after.rsplit(":", 1)[1]) if after else 0
        source = self.thread_comments if field == "nested" else self.review_comments[field[7:]] if field.startswith("review:") else self.collections[field]
        nodes = deepcopy(source[start:start + 100])
        if field == "reviewThreads":
            for node in nodes:
                node["comments"] = self.page("nested") if node["id"] == "thread-0" else {
                    "nodes": [], "totalCount": 0, "pageInfo": {"hasNextPage": False, "endCursor": None}}
        if field == "reviews":
            for node in nodes:
                if node["id"] in self.review_comments:
                    node["comments"] = self.page("review:" + node["id"])
        return {"nodes": nodes, "totalCount": len(source), "pageInfo": {
            "hasNextPage": start + 100 < len(source), "endCursor": f"{field}:{start + len(nodes)}"}}

    def graphql(self, query, variables):
        self.calls.append((query, dict(variables)))
        if "review" in variables:
            result = {"node": {"id": variables["review"], "pullRequest": {"headRefOid": self.head},
                "comments": self.page("review:" + variables["review"], variables.get("after"))}}
        elif "thread" in variables:
            result = {"node": {"id": variables["thread"], "isResolved": True,
                "pullRequest": {"headRefOid": self.head}, "comments": self.page("nested", variables.get("after"))}}
        else:
            pull = {"headRefOid": self.head}
            fields = [re.search(r"(\w+)\(first:100,after:\$after\)", query)[1]] if "after" in variables else list(self.collections)
            for field in fields:
                value = self.page(field, variables.get("after"))
                if field == "contexts":
                    pull["commits"] = {"nodes": [{"commit": {"statusCheckRollup": {"contexts": value}}}]}
                else:
                    pull[field] = value
            result = {"repository": {"pullRequest": pull}}
        self.mutate(query, variables, result)
        return result


class ReviewPaginationTest(unittest.TestCase):
    def test_collects_all_connections_and_nested_replies_before_judgment(self):
        gh = PagedGH()
        observation = review.review_snapshot(gh, "owner/repo", 1, POLICY, include_bodies=True)
        self.assertEqual(review.classify("head", observation)[0], "judgment-required")
        self.assertEqual({s["id"] for s in observation["findingSurfaces"]}, {"comment-200", "review-200", "thread-100"})
        self.assertEqual(len(observation["checks"]), 101)
        self.assertEqual(len(observation["threads"]), 101)
        self.assertEqual(len(observation["unclassifiedTopLevelComments"]), 201)
        first_thread = next(t for t in observation["threads"] if t["id"] == "thread-0")
        self.assertTrue(first_thread["maintainerReply"])
        self.assertEqual(len(first_thread["comments"]), 201)
        self.assertEqual(observation["unansweredAutomationThreads"], 0)
        self.assertEqual(observation["unresolvedThreads"], 1)
        self.assertEqual(len(gh.calls), 18)

    def test_refuses_partial_or_non_advancing_pages(self):
        from kgr_github import WaitError
        for failure in ("cursor", "pageInfo", "count", "duplicate", "null-node", "missing-id"):
            with self.subTest(failure=failure):
                gh = PagedGH()
                def mutate(query, variables, result):
                    if variables.get("after") != "comments:100":
                        return
                    page = result["repository"]["pullRequest"]["comments"]
                    if failure == "cursor":
                        page["pageInfo"]["endCursor"] = "comments:100"
                    elif failure == "pageInfo":
                        del page["pageInfo"]
                    elif failure == "count":
                        page["totalCount"] += 1
                    elif failure == "duplicate":
                        page["nodes"][0]["id"] = "comment-0"
                    elif failure == "null-node":
                        page["nodes"][0] = None
                    else:
                        del page["nodes"][0]["id"]
                gh.mutate = mutate
                with self.assertRaises(WaitError):
                    review.review_snapshot(gh, "owner/repo", 1, POLICY)

    def test_changed_head_or_thread_resolution_invalidates_pagination(self):
        from kgr_github import TransientError
        for field in ("head", "resolved"):
            with self.subTest(field=field):
                gh = PagedGH()
                def mutate(query, variables, result):
                    if field == "head" and variables.get("after") == "comments:100":
                        result["repository"]["pullRequest"]["headRefOid"] = "new-head"
                    if field == "resolved" and variables.get("thread"):
                        result["node"]["isResolved"] = False
                gh.mutate = mutate
                with self.assertRaises(TransientError):
                    review.review_snapshot(gh, "owner/repo", 1, POLICY)

    def test_same_head_edit_between_complete_censuses_requires_a_fresh_read(self):
        from kgr_github import TransientError
        gh = PagedGH()
        passes = 0
        def mutate(query, variables, result):
            nonlocal passes
            if "after" not in variables:
                passes += 1
            if passes == 2 and variables.get("after") == "comments:200":
                result["repository"]["pullRequest"]["comments"]["nodes"][0]["body"] = "edited finding"
        gh.mutate = mutate
        with self.assertRaises(TransientError):
            review.review_snapshot(gh, "owner/repo", 1, POLICY)

    def test_review_comment_pagination_distinguishes_reply_transport_from_verdict(self):
        for reply_only in (False, True):
            with self.subTest(reply_only=reply_only):
                gh = PagedGH()
                gh.collections["reviews"][-2] = verdict("pending", T2, "rate limit", "COMMENTED")
                gh.collections["reviews"][-1] = verdict("latest", T3, state="COMMENTED")
                gh.review_comments["latest"] = [{"id": f"review-comment-{i}",
                    "replyTo": {"id": "original"} if i < 100 or reply_only else None} for i in range(101)]
                value = review.review_snapshot(gh, "owner/repo", 1, POLICY)
                self.assertEqual(value["automations"][0]["terminal"], not reply_only)
                self.assertEqual(sum("review" in variables for _, variables in gh.calls), 2)


if __name__ == "__main__":
    unittest.main()
