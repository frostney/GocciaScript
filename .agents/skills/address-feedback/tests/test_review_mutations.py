"""Mutation receipts must identify the actual authorized GitHub action."""
import sys
import tempfile
import unittest
from copy import deepcopy
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "scripts"))
sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "delivery-wait/scripts"))
from review_mutations import reply, resolve
from kgr_github import WaitError


class GH:
    def __init__(self):
        self.head = "head"
        self.actor = "maintainer"
        self.comments = {10: {"id": 10, "pull_request_url": "https://api.github.com/repos/owner/repo/pulls/7",
            "body": "finding", "user": {"login": "reviewer"}}}
        self.thread = {"id": "thread", "isResolved": False, "pullRequest": {
            "number": 7, "headRefOid": "head", "repository": {"nameWithOwner": "owner/repo"}}}
        self.posts = 0
        self.resolutions = 0
        self.hide_replies = False
        self.fail_post = None
        self.fail_resolve = False
        self.drift_after_write = False
        self.lookup_id = None

    def graphql(self, query, variables):
        if "resolveReviewThread" in query:
            self.resolutions += 1
            self.thread["isResolved"] = True
            if self.drift_after_write:
                self.thread["pullRequest"]["headRefOid"] = "new-head"
            if self.fail_resolve:
                raise WaitError("response lost after resolve")
            return {"resolveReviewThread": {"thread": {"id": "untrusted", "isResolved": True}}}
        if "node(id:" in query:
            return {"node": deepcopy(self.thread)}
        return {"repository": {"pullRequest": {"headRefOid": self.head}}}

    def rest(self, path, method="GET", fields=None):
        if path == "user":
            return {"login": self.actor}
        if method == "POST":
            self.posts += 1
            if self.fail_post != "before":
                self.comments[20] = {"id": 20, "pull_request_url": self.comments[10]["pull_request_url"],
                    "in_reply_to_id": 10, "body": fields["body"], "user": {"login": self.actor}}
            if self.drift_after_write:
                self.head = "new-head"
            if self.fail_post:
                raise WaitError("reply response unavailable")
            return {"id": 20}
        value = deepcopy(self.comments[int(path.rsplit("/", 1)[1])])
        if self.lookup_id is not None and value["id"] == 20:
            value["id"] = self.lookup_id
        return value

    def rest_pages(self, path):
        return [[deepcopy(c) for c in self.comments.values() if not self.hide_replies or c["id"] == 10]]


class MutationTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.state = Path(self.temp.name) / "reply.json"
        self.gh = GH()

    def tearDown(self):
        self.temp.cleanup()

    def reply(self, **changes):
        args = dict(gh=self.gh, repo="owner/repo", pr=7, head="head", comment_id=10,
                    body="Fixed with verified regression coverage.", operation_id="operation", state_path=self.state)
        return reply(**(args | changes))

    def test_reply_reuses_only_the_verified_same_request(self):
        first = self.reply()
        second = self.reply()
        self.assertEqual(first["state"], "satisfied")
        self.assertTrue(first["observation"]["created"])
        self.assertEqual(second["observation"]["commentId"], 20)
        self.assertFalse(second["observation"]["created"])
        self.assertEqual(self.gh.posts, 1)
        self.assertNotIn("Fixed with verified regression coverage", self.state.read_text())

    def test_operation_cannot_be_reused_for_different_body_or_target(self):
        self.reply()
        with self.assertRaises(WaitError):
            self.reply(body="Different disposition")
        self.gh.comments[11] = dict(self.gh.comments[10], id=11)
        with self.assertRaises(WaitError):
            self.reply(comment_id=11)
        self.assertEqual(self.gh.posts, 1)

    def test_marker_collision_with_other_author_target_or_body_is_rejected(self):
        self.reply()
        original = deepcopy(self.gh.comments[20])
        for mutation in (lambda x: x.update(user={"login": "other"}),
                         lambda x: x.update(in_reply_to_id=999),
                         lambda x: x.update(body=x["body"] + "edited")):
            with self.subTest(mutation=mutation):
                self.gh.comments[20] = deepcopy(original)
                mutation(self.gh.comments[20])
                with self.assertRaises(WaitError):
                    self.reply()
        self.assertEqual(self.gh.posts, 1)

    def test_cross_pr_reply_target_never_posts(self):
        self.gh.comments[10]["pull_request_url"] = "https://api.github.com/repos/owner/repo/pulls/8"
        with self.assertRaises(WaitError):
            self.reply()
        self.assertEqual(self.gh.posts, 0)

    def test_lost_post_response_reconciles_without_duplicate(self):
        self.gh.fail_post = "after"
        result = self.reply()
        self.assertEqual(result["state"], "satisfied")
        self.assertIsNone(result["observation"]["created"])
        self.assertEqual(self.reply()["state"], "satisfied")
        self.assertEqual(self.gh.posts, 1)

    def test_missing_receipt_is_pending_and_never_blindly_reposts(self):
        self.gh.fail_post = "after"
        self.gh.hide_replies = True
        self.assertEqual(self.reply()["state"], "pending")
        self.assertEqual(self.reply()["state"], "pending")
        self.gh.hide_replies = False
        self.assertEqual(self.reply()["state"], "satisfied")
        self.assertEqual(self.gh.posts, 1)

    def test_failed_write_without_receipt_does_not_assume_absence(self):
        self.gh.fail_post = "before"
        self.assertEqual(self.reply()["state"], "pending")
        self.gh.fail_post = None
        self.assertEqual(self.reply()["state"], "pending")
        self.assertEqual(self.gh.posts, 1)

    def test_wrong_lookup_identity_cannot_confirm_a_reply(self):
        self.gh.lookup_id = 999
        self.assertEqual(self.reply()["state"], "pending")
        self.assertEqual(self.gh.posts, 1)

    def test_head_drift_after_post_preserves_receipt_without_claiming_readiness(self):
        self.gh.drift_after_write = True
        result = self.reply()
        self.assertEqual(result["state"], "invalidated")
        self.assertEqual(result["observation"]["commentId"], 20)
        self.assertEqual(self.reply()["state"], "invalidated")
        self.assertEqual(self.gh.posts, 1)

    def test_cross_repository_or_pr_thread_never_resolves(self):
        for repo, pr in (("other/repo", 7), ("owner/repo", 8)):
            with self.subTest(repo=repo, pr=pr):
                with self.assertRaises(WaitError):
                    resolve(self.gh, repo, pr, "head", "thread")
        self.assertEqual(self.gh.resolutions, 0)

    def test_resolution_uses_independent_current_thread_state(self):
        self.gh.fail_resolve = True
        value = resolve(self.gh, "owner/repo", 7, "head", "thread")
        self.assertEqual(value["state"], "satisfied")
        self.assertEqual(value["observation"]["threadId"], "thread")
        self.assertEqual(resolve(self.gh, "owner/repo", 7, "head", "thread")["state"], "satisfied")
        self.assertEqual(self.gh.resolutions, 1)

    def test_resolution_during_head_change_is_invalidated(self):
        self.gh.drift_after_write = True
        self.assertEqual(resolve(self.gh, "owner/repo", 7, "head", "thread")["state"], "invalidated")
        self.assertEqual(self.gh.resolutions, 1)


if __name__ == "__main__":
    unittest.main()
