import codecs
import importlib.util
import json
import os
from pathlib import Path
import stat
import subprocess
import sys
import tempfile
import textwrap
import threading
import time
import unittest

HELPER = Path(__file__).resolve().parents[1] / "scripts/publication_guard.py"
PRIVATE = "example-private/secret-repo"
PUBLIC = "octocat/Hello-World"
SHA = "3f786850e387550fdab836ed7e6dc881de23001b"
GONE = "example-gone/vanished"  # Answers 404, so a URL to it counts as private.
FLAKY = "example-flaky/tool"  # Answers 502.
SPEC = importlib.util.spec_from_file_location("publication_guard", HELPER)
publication_guard = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(publication_guard)

# The guard runs as an invented user; its home is outside /Users and /home so
# the home directory and the username rules can fail separately.
HOME = "/srv/example-home"
USER = "example-user"
TMPDIR = "/private/var/example-tmp/T"
# Built from parts so that committing this file through the guard keeps them as written.
AGENT_TMP = "/tmp/" + "claude-1000"
MAC_TMP = "/var/" + "folders/ab/cd123ef/T"
ENCODED = "-" + "Users-example-user-Documents-Projects-example-lifecycle-example-project--claude-worktrees-example-worktree"
SESSION = f"/private{AGENT_TMP}/{ENCODED}/0b6f5a2e-1c3d-4e5f-8a9b-0c1d2e3f4a5b"

SECRET_GIST = "0123456789abcdef0123456789abcdef"
PUBLIC_GIST = "fedcba9876543210fedcba9876543210"

# Repositories absent from this table answer 404; example-flaky answers 502, and
# FAKE_GH_FAIL names further calls that answer 502: user, orgs, or lookup (every
# reference lookup, while the destination check still answers).
FAKE_GH = textwrap.dedent(f"""\
    #!{sys.executable}
    import json, os, sys
    args = sys.argv[1:]
    with open(os.environ["FAKE_GH_LOG"], "a") as log:
        log.write(" ".join(args) + "\\n")
    host = "github.com"
    if "--hostname" in args:
        at = args.index("--hostname")
        host = args[at + 1]
        del args[at:at + 2]
    fail = os.environ.get("FAKE_GH_FAIL", "").split(",")
    visibility = {{"example-private/secret-repo": "private", "octocat/hello-world": "public",
                   "example-public/open-repo": "public"}}
    if host != "github.com":
        visibility = {{"example-org/example-tool": "internal"}}
    gists = {{"{SECRET_GIST}": False, "{PUBLIC_GIST}": True}}

    def bad_gateway():
        sys.exit(print("gh: Bad Gateway (HTTP 502)", file=sys.stderr) or 1)

    if args[:2] == ["repo", "view"]:
        print(os.environ.get("FAKE_GH_REPO", "octocat/Hello-World"))
    elif args[:2] == ["api", "user"]:
        if "user" in fail:
            bad_gateway()
        print("example-user")
    elif args[:3] == ["api", "--paginate", "user/orgs"]:
        if "orgs" in fail:
            bad_gateway()
        print("example-private")
    elif args[0] == "api" and args[1].startswith("gists/"):
        gist = args[1].removeprefix("gists/")
        if gist not in gists:
            sys.exit(print("gh: Not Found (HTTP 404)", file=sys.stderr) or 1)
        print(json.dumps({{"id": gist, "public": gists[gist]}}))
    elif args[0] == "api" and args[1].startswith("repos/"):
        slug = args[1].removeprefix("repos/").lower()
        if slug.startswith("example-flaky/") or ("lookup" in fail and "--jq" not in args):
            bad_gateway()
        if slug not in visibility:
            sys.exit(print("gh: Not Found (HTTP 404)", file=sys.stderr) or 1)
        body = {{"visibility": visibility[slug], "private": visibility[slug] != "public"}}
        print(body["visibility"] if "--jq" in args else json.dumps(body))
    else:
        sys.exit(print("unexpected fake gh call: " + repr(args), file=sys.stderr) or 1)
    """)
# Resolves SSH aliases the way ~/.ssh/config Host entries would. An HTTPS remote
# on code.example.invalid must never be looked up here.
FAKE_SSH = textwrap.dedent(f"""\
    #!{sys.executable}
    import sys
    alias = sys.argv[-1]
    print("hostname " + ("github.com" if alias in {{"github-example", "code.example.invalid"}} else alias))
    """)


class PublicationGuardTests(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory(prefix="kgr-publication-guard-")
        self.addCleanup(temporary.cleanup)
        self.base = Path(temporary.name).resolve()
        bin_dir = self.base / "bin"
        bin_dir.mkdir()
        (bin_dir / "gh").write_text(FAKE_GH)
        (bin_dir / "gh").chmod(0o755 | stat.S_IXUSR)
        (bin_dir / "ssh").write_text(FAKE_SSH)
        (bin_dir / "ssh").chmod(0o755 | stat.S_IXUSR)
        self.log = self.base / "gh.log"
        self.env = {**os.environ, "PATH": f"{bin_dir}{os.pathsep}{os.environ['PATH']}", "FAKE_GH_LOG": str(self.log)}
        self.guard_env = {key: value for key, value in self.env.items() if key not in {"TEMP", "TMP", "USERPROFILE"}}
        self.guard_env.update({"HOME": HOME, "TMPDIR": TMPDIR, **{name: USER for name in ("LOGNAME", "USER", "LNAME", "USERNAME")}})
        self.repo = self.base / "repository"
        self.repo.mkdir()
        self.git("init", "--initial-branch=main")
        self.git("config", "user.name", "Fixture")
        self.git("config", "user.email", "fixture@example.invalid")
        self.git("commit", "--allow-empty", "-m", "initial")
        self.git("remote", "add", "origin", f"https://github.com/{PUBLIC}.git")

    def git(self, *args):
        result = subprocess.run(["git", *args], cwd=self.repo, capture_output=True, text=True, env=self.env)
        self.assertEqual(result.returncode, 0, result.stderr)
        return result.stdout

    def run_guard(self, *args, env=None):
        return subprocess.run([sys.executable, str(HELPER), *args], cwd=self.repo, capture_output=True, text=True,
                              env={**self.guard_env, **(env or {})})

    def guard(self, *args, repo=None, env=None):
        """Runs the guard against a PR base REPO, or with origin, the push remote, pointing at REPO."""
        if args[0] == "pr":
            args = (*args, "--repo", repo or PUBLIC)
        elif repo:
            self.git("remote", "set-url", "origin", f"https://github.com/{repo}.git")
        result = self.run_guard(*args, env=env)
        self.assertEqual(result.returncode, 0, result.stderr)
        if (repo or PUBLIC) == PUBLIC:
            self.assertNotIn("secret-repo", result.stdout)
            self.assertNotIn(USER, result.stdout)
        return result.stdout

    def stage(self, files):
        for name, content in files.items():
            path = self.repo / name
            path.parent.mkdir(parents=True, exist_ok=True)
            if isinstance(content, bytes):
                path.write_bytes(content)
            else:
                path.write_text(content if isinstance(content, str) else json.dumps(content, indent=2) + "\n")
        self.git("add", *files)

    def staged(self, name):
        return self.git("show", f":{name}")

    def staged_bytes(self, name):
        return subprocess.run(["git", "cat-file", "blob", f":{name}"], cwd=self.repo, capture_output=True,
                              check=True).stdout

    def staged_json(self, name):
        return json.loads(self.staged(name))

    def write(self, name, text):
        path = self.base / name
        path.write_text(text)
        return path

    def test_private_destination_is_a_no_op(self):
        other = f"https://github.com/{GONE}/pull/2"
        notes = f"See {PRIVATE}#4 and {other} in /Users/{USER}/notes.md.\n"
        self.stage({"a.json": {"repo": PRIVATE, "pr": 4, "from": other}, "notes.md": notes})
        message = self.write("message", f"fix: sync {PRIVATE} from {other} via {SESSION}/out.txt\n")
        output = self.guard("staged", "--message-file", str(message), repo=PRIVATE)
        self.assertIn("not public", output)
        self.assertEqual(self.staged_json("a.json"), {"repo": PRIVATE, "pr": 4, "from": other})
        self.assertEqual(self.staged("notes.md"), notes)
        self.assertEqual(message.read_text(), f"fix: sync {PRIVATE} from {other} via {SESSION}/out.txt\n")
        self.assertEqual(self.log.read_text().count("repos/"), 1)

    def test_public_reference_is_untouched(self):
        text = (f"{PUBLIC} and {PUBLIC}#12 and https://github.com/{PUBLIC}/pull/3\n"
                f"[link](https://github.com/{PUBLIC}/issues/7), https://github.com/{PUBLIC}.git and "
                "https://github.com/orgs/example-private/projects/1\n")
        self.stage({"notes.md": text, "a.json": {"repo": PUBLIC, "pr": 12, "head": SHA}})
        self.assertIn("nothing to rewrite", self.guard("staged"))
        self.assertEqual(self.staged("notes.md"), text)
        self.assertEqual(self.staged_json("a.json"), {"repo": PUBLIC, "pr": 12, "head": SHA})

    def test_private_slug_in_json_maps_to_a_stable_placeholder_across_files_and_runs(self):
        self.stage({"one.json": {"repo": PRIVATE}, "two.json": {"source": f"https://github.com/{PRIVATE}"},
                    "three.json": {"repo": "example-private/other-thing"}})
        output = self.guard("staged")
        self.assertIn("placeholder example-org/example-repo-1", output)
        self.assertEqual(self.staged_json("one.json"), {"repo": "example-org/example-repo-1"})
        self.assertTrue(self.staged("one.json").startswith('{\n  "repo"'))
        self.assertEqual(self.staged_json("two.json"), {"source": "https://github.com/example-org/example-repo-1"})
        self.assertEqual(self.staged_json("three.json"), {"repo": "example-org/example-repo-2"})
        self.git("commit", "-m", "first")
        self.stage({"four.json": {"first": "example-private/other-thing", "repo": PRIVATE.upper()}})
        self.guard("staged")
        self.assertEqual(self.staged_json("four.json"),
                         {"first": "example-org/example-repo-2", "repo": "example-org/example-repo-1"})

    def test_pr_numbers_and_shas_are_remapped_consistently(self):
        self.stage({
            "a.json": {"repo": PRIVATE, "pr": 41, "head": SHA, "url": f"https://github.com/{PRIVATE}/pull/41",
                       "commit": f"https://github.com/{PRIVATE}/commit/{SHA}", "id": 41},
            "b.json": {"ref": f"{PRIVATE}#41", "short": SHA[:7], "pullRequests": [42, 41]},
        })
        self.guard("staged")
        a, b = self.staged_json("a.json"), self.staged_json("b.json")
        mapped = a["head"]
        self.assertNotEqual(mapped, SHA)
        self.assertRegex(mapped, r"^[0-9a-f]{40}$")
        self.assertEqual(a["pr"], 1)
        self.assertEqual(a["id"], 41)
        self.assertEqual(a["url"], "https://github.com/example-org/example-repo-1/pull/1")
        self.assertEqual(a["commit"], f"https://github.com/example-org/example-repo-1/commit/{mapped}")
        self.assertEqual(b["ref"], "example-org/example-repo-1#1")
        self.assertEqual(b["short"], mapped[:7])
        self.assertEqual(b["pullRequests"], [2, 1])

    def test_free_text_is_stubbed_while_kept_lines_survive(self):
        (self.repo / ".github").mkdir()
        (self.repo / ".github/publication-guard.json").write_text(json.dumps({"keep": ["^Ticket:"]}))
        text = ("**Status:** merged\nThe launch plan for the partner stays confidential\n"
                "Ticket: rollout\nBuild: passed\nDuration 12 ms\n\nAnother private sentence here")
        self.stage({"a.json": {"about": f"Recorded from {PRIVATE}.", "variants": [{"text": text, "title": "Short"}]},
                    "b.json": {"text": "A long sentence unrelated to any private repository at all"}})
        output = self.guard("staged")
        variant = self.staged_json("a.json")["variants"][0]
        self.assertEqual(variant["text"], "**Status:** merged\n[redacted]\nTicket: rollout\nBuild: passed\n"
                                          "Duration 12 ms\n\n[redacted]")
        self.assertEqual(variant["title"], "Short")
        self.assertEqual(self.staged_json("a.json")["about"], "[redacted]")
        self.assertEqual(self.staged_json("b.json")["text"], "A long sentence unrelated to any private repository at all")
        self.assertIn("stubbed lines 3", output)

    def test_prose_references_are_neutralised(self):
        self.stage({"notes.md": textwrap.dedent(f"""\
            Ported from {PRIVATE}.
            Fixes {PRIVATE}#12 and see https://github.com/{PRIVATE}/pull/3.
            Bug: https://github.com/{PRIVATE}/issues/9, commit https://github.com/{PRIVATE}/commit/{SHA}
            Read [the thread](https://github.com/{PRIVATE}/pull/3#discussion_r1) and `{PRIVATE}`.
            Keep {PUBLIC}#1.
            """)})
        self.guard("staged")
        self.assertEqual(self.staged("notes.md"), textwrap.dedent(f"""\
            Ported from a private repository.
            Fixes an issue or pull request in a private repository and see a pull request in a private repository.
            Bug: an issue in a private repository, commit a commit in a private repository
            Read a pull request in a private repository and `a private repository`.
            Keep {PUBLIC}#1.
            """))
        self.assertEqual((self.repo / "notes.md").read_text(), self.staged("notes.md"))

    def test_unresolvable_reference_counts_as_private(self):
        self.stage({"notes.md": f"From https://github.com/{GONE}/pull/9 and {GONE}, "
                                "plus example-private/deleted-repo.\n"})
        self.guard("staged")
        self.assertEqual(self.staged("notes.md"), "From a pull request in a private repository and a private "
                                                  "repository, plus a private repository.\n")

    def test_word_pairs_and_paths_are_not_references(self):
        (self.repo / "docs").mkdir()
        (self.repo / "docs/guide.md").write_text("guide\n")
        (self.repo / "example-private").mkdir()
        (self.repo / "example-private/notes.md").write_text("notes\n")
        text = "Use and/or input/output, 09/30, docs/guide.md, example-private/notes.md, ./src/app and @scope/pkg.\n"
        self.stage({"notes.md": text})
        self.assertIn("nothing to rewrite", self.guard("staged"))
        self.assertEqual(self.staged("notes.md"), text)
        lookups = self.log.read_text()
        self.assertIn("repos/and/or", lookups)
        self.assertNotIn("repos/09/30", lookups)
        self.assertNotIn("repos/docs/guide.md", lookups)

    def test_commit_message_is_rewritten_before_the_commit_is_made(self):
        self.stage({"a.json": {"repo": PRIVATE}})
        message = self.write("message", f"fix: port retry from {PRIVATE}#7\n\nSee https://github.com/{PRIVATE}/pull/7\n")
        output = self.guard("staged", "--message-file", str(message))
        self.assertIn("rewrote commit message", output)
        self.git("commit", "-F", str(message))
        self.assertEqual(self.git("log", "-1", "--format=%B").strip(),
                         "fix: port retry from an issue or pull request in a private repository\n\n"
                         "See a pull request in a private repository")
        self.assertEqual(json.loads(self.git("show", "HEAD:a.json")), {"repo": "example-org/example-repo-1"})

    def test_pr_title_and_body_are_rewritten(self):
        title = self.write("title", f"feat: match {PRIVATE} retries")
        body = self.write("body", f"Mirrors https://github.com/{PRIVATE}/pull/5.\n\nCloses #3\n")
        output = self.guard("pr", "--title-file", str(title), "--body-file", str(body))
        self.assertIn("rewrote PR title", output)
        self.assertIn("rewrote PR body", output)
        self.assertEqual(title.read_text(), "feat: match a private repository retries")
        self.assertEqual(body.read_text(), "Mirrors a pull request in a private repository.\n\nCloses #3\n")

    def test_outgoing_diff_is_rewritten_and_staged(self):
        self.stage({"notes.md": f"Copied from {PRIVATE}.\n"})
        self.git("commit", "-m", "unguarded")
        self.guard("outgoing", "--base", "HEAD~1")
        self.assertEqual(self.staged("notes.md"), "Copied from a private repository.\n")
        self.assertEqual((self.repo / "notes.md").read_text(), "Copied from a private repository.\n")

    def test_second_run_changes_nothing_more(self):
        self.stage({"a.json": {"about": f"From {PRIVATE} with a long free-text sentence", "pr": 8, "head": SHA,
                               "url": f"https://github.com/{PRIVATE}/pull/8", "log": f"{SESSION}/run.log"},
                    "notes.md": f"See {PRIVATE}#8 and {self.repo}/notes.md in /Users/{USER}.\n"})
        message = self.write("message", f"fix: sync {PRIVATE} from {HOME}/notes.md\n")
        body = self.write("body", f"See https://github.com/{PRIVATE}/pull/8\n")
        self.guard("staged", "--message-file", str(message))
        self.guard("pr", "--body-file", str(body))
        first = (self.staged("a.json"), self.staged("notes.md"), message.read_text(), body.read_text())
        self.assertIn("nothing to rewrite", self.guard("staged", "--message-file", str(message)))
        self.assertIn("nothing to rewrite", self.guard("pr", "--body-file", str(body)))
        self.assertEqual((self.staged("a.json"), self.staged("notes.md"), message.read_text(), body.read_text()), first)

    def test_home_directory_and_username_become_a_tilde(self):
        self.stage({"notes.md": textwrap.dedent(f"""\
            Config at {HOME}/.config/example-tool.toml.
            Notes in /Users/{USER}/Documents/notes.md, /home/{USER}/src/app.py and C:\\Users\\{USER}\\AppData\\log.txt.
            Session log: {HOME}/.claude/projects/-srv-example-home-work-example-project/log.jsonl
            Untouched: /Users/{USER}name/a, /home/user/b and ~/c.
            """)})
        self.assertIn("local paths 6", self.guard("staged"))
        self.assertEqual(self.staged("notes.md"), textwrap.dedent("""\
            Config at ~/.config/example-tool.toml.
            Notes in ~/Documents/notes.md, ~/src/app.py and ~\\AppData\\log.txt.
            Session log: ~/.claude/projects/example-project/log.jsonl
            Untouched: /Users/example-username/a, /home/user/b and ~/c.
            """))

    def test_temporary_folders_become_tmp(self):
        self.stage({"notes.md": textwrap.dedent(f"""\
            Answer cites [create-pr/SKILL.md]({SESSION}/scratchpad/create-pr/SKILL.md).
            Also {AGENT_TMP}/build/out.txt, {MAC_TMP}/report.txt, /private{MAC_TMP}/x.txt,
            /var/example-tmp/T/cache/a.bin and {TMPDIR}/b.bin.
            Untouched: /tmp/scratch.txt, /tmp/claude-notes.txt and /private/tmp/run.log.
            """)})
        self.guard("staged")
        self.assertEqual(self.staged("notes.md"), textwrap.dedent("""\
            Answer cites [create-pr/SKILL.md](/tmp/example-project/0b6f5a2e-1c3d-4e5f-8a9b-0c1d2e3f4a5b/scratchpad/create-pr/SKILL.md).
            Also /tmp/build/out.txt, /tmp/report.txt, /tmp/x.txt,
            /tmp/cache/a.bin and /tmp/b.bin.
            Untouched: /tmp/scratch.txt, /tmp/claude-notes.txt and /private/tmp/run.log.
            """))

    def test_checkout_and_worktree_paths_become_relative_in_json(self):
        worktree = self.base / "example-worktree"
        self.git("worktree", "add", "-b", "example-branch", str(worktree))
        alias = str(self.repo).removeprefix("/private")  # macOS also spells /private/var as /var.
        self.stage({"evals/answers.json": {
            "answers": [{"text": f"Edited [SKILL.md]({self.repo}/create-pr/SKILL.md) and {worktree}/evals/a.json",
                         "cwd": str(self.repo), "guide": f"{alias}/docs/guide.md"}],
            "windows": f"C:\\Users\\{USER}\\notes.txt"}})
        self.guard("staged")
        expected = {"answers": [{"text": "Edited [SKILL.md](./create-pr/SKILL.md) and ./evals/a.json",
                                 "cwd": "./", "guide": "./docs/guide.md"}], "windows": "~\\notes.txt"}
        self.assertEqual(self.staged("evals/answers.json"), json.dumps(expected, indent=2) + "\n")

    def test_main_checkout_outside_home_and_temporary_folders_stays_literal(self):
        container = publication_guard.LocalPaths("/srv/example-app", ["/srv/example-app/.claude/worktrees/example-worktree"],
                                                 [], [f"/home/{USER}"], USER)
        self.assertEqual(container.rewrite("WORKDIR /srv/example-app\n/srv/example-app/.claude/worktrees/example-worktree/a.md"),
                         ("WORKDIR /srv/example-app\n./a.md", 1))
        personal = publication_guard.LocalPaths(f"/home/{USER}/src/example-app", [], [], [f"/home/{USER}"], USER)
        self.assertEqual(personal.rewrite(f"/home/{USER}/src/example-app/a.md and /home/{USER}/b"), ("./a.md and ~/b", 2))
        scratch = publication_guard.LocalPaths("/var/tmp/example-app")
        self.assertEqual(scratch.rewrite("/var/tmp/example-app/a.md"), ("./a.md", 1))

    def test_windows_home_and_resolved_spellings_are_recognised(self):
        windows = publication_guard.LocalPaths(homes=[f"C:\\Users\\{USER}"])
        self.assertEqual(windows.rewrite(f"C:/Users/{USER}/a and c:\\users\\{USER}\\b"), ("~/a and ~\\b", 2))
        target = self.base / "session-target"
        target.mkdir()
        (self.base / "session-link").symlink_to(target)
        linked = publication_guard.LocalPaths(temp_dirs=[str(self.base / "session-link")])
        self.assertEqual(linked.rewrite(f"{target}/log.txt"), ("/tmp/log.txt", 1))
        self.assertEqual(publication_guard.LocalPaths(temp_dirs=["/tmp"]).rewrite("/tmp/log.txt"), ("/tmp/log.txt", 0))

    def test_local_paths_leave_commit_messages_and_pr_metadata(self):
        message = self.write("message", f"fix: read /Users/{USER}/notes.txt\n")
        self.assertIn("rewrote commit message: local paths 1", self.guard("staged", "--message-file", str(message)))
        self.assertEqual(message.read_text(), "fix: read ~/notes.txt\n")
        title = self.write("title", f"docs: logs in {SESSION}")
        body = self.write("body", f"Logs: {SESSION}/out.txt\n")
        self.guard("pr", "--title-file", str(title), "--body-file", str(body))
        self.assertEqual(title.read_text(), "docs: logs in /tmp/example-project/0b6f5a2e-1c3d-4e5f-8a9b-0c1d2e3f4a5b")
        self.assertEqual(body.read_text(), "Logs: /tmp/example-project/0b6f5a2e-1c3d-4e5f-8a9b-0c1d2e3f4a5b/out.txt\n")

    def test_outgoing_names_commits_that_already_hold_the_original_text(self):
        self.stage({"notes.md": f"Read /Users/{USER}/notes.txt\n", "ported.md": f"Ported from {PRIVATE}.\n"})
        self.git("commit", "-m", "docs: add notes")
        leaked = self.git("rev-parse", "--short=7", "HEAD").strip()
        self.stage({"notes.md": "Read the notes\n"})
        self.git("rm", "-q", "ported.md")
        self.git("commit", "-m", "docs: drop the path")
        clean = self.git("rev-parse", "--short=7", "HEAD").strip()
        self.stage({"other.md": f"Ported from /home/{USER}/src.\n"})
        self.git("commit", "-m", f"docs: port from https://github.com/{GONE}/pull/2")
        both = self.git("rev-parse", "--short=7", "HEAD").strip()
        output = self.guard("outgoing", "--base", "HEAD~3")
        self.assertIn(f"commit {leaked} adds a private reference or local path to notes.md, ported.md; "
                      "pushing publishes that commit unchanged", output)
        self.assertIn(f"commit {both} adds a private reference or local path to other.md, its message", output)
        self.assertNotIn(clean, output)
        self.assertNotIn("earlier commits on the branch", output)
        self.assertEqual(self.staged("notes.md"), "Read the notes\n")
        self.assertEqual(self.staged("other.md"), "Ported from ~/src.\n")

    def test_unknown_destination_is_an_operational_error(self):
        result = subprocess.run([sys.executable, str(HELPER), "pr", "--repo", "example-gone/nowhere"],
                                cwd=self.repo, capture_output=True, text=True, env=self.guard_env)
        self.assertEqual(result.returncode, 2)
        self.assertIn("cannot ask GitHub", result.stderr)


    def test_destination_is_the_push_remote_not_the_gh_default(self):
        self.stage({"notes.md": f"Ported from {PRIVATE}.\n"})
        self.git("remote", "add", "upstream", f"git@github.com:{PRIVATE}.git")
        gh_default_is_private = {"FAKE_GH_REPO": PRIVATE}
        self.git("config", "branch.main.pushRemote", "upstream")
        self.assertIn(f"{PRIVATE} is not public", self.run_guard("staged", env=gh_default_is_private).stdout)
        self.assertIn("is public", self.guard("staged", "--remote", "origin", env=gh_default_is_private))
        self.assertEqual(self.staged("notes.md"), "Ported from a private repository.\n")
        self.git("config", "--unset", "branch.main.pushRemote")
        self.git("config", "remote.pushDefault", "alias")
        self.git("remote", "add", "alias", f"git@github-example:{PUBLIC}.git")
        self.assertIn(f"{PUBLIC} is public", self.guard("staged", env=gh_default_is_private))
        self.git("config", "remote.pushDefault", "enterprise")
        self.git("remote", "add", "enterprise", "https://code.example.invalid/example-org/example-tool.git")
        self.assertIn("code.example.invalid/example-org/example-tool is not public", self.guard("staged"))
        self.git("remote", "set-url", "enterprise", f"ssh://git@ssh.github.com:443/{PUBLIC}.git")
        self.assertIn(f"{PUBLIC} is public", self.guard("staged"))
        self.assertNotIn("repo view", self.log.read_text())
        self.git("config", "remote.pushDefault", "nowhere")
        result = self.run_guard("staged")
        self.assertEqual(result.returncode, 2)
        self.assertIn("push remote 'nowhere' does not exist", result.stderr)

    def test_pr_destination_is_the_base_repository_given(self):
        body = self.write("body", f"See {PRIVATE}#3\n")
        self.assertEqual(self.run_guard("pr", "--body-file", str(body)).returncode, 2)
        self.assertIn(f"{PRIVATE} is not public", self.guard("pr", "--body-file", str(body), repo=PRIVATE))
        self.assertIn("is not public", self.guard("pr", "--body-file", str(body),
                                                  repo="code.example.invalid/example-org/example-tool"))
        self.assertIn(f"{PUBLIC} is public", self.guard("pr", "--body-file", str(body), repo=f"github.com/{PUBLIC}"))
        self.assertEqual(body.read_text(), "See an issue or pull request in a private repository\n")

    def test_json_escapes_and_keys_get_the_same_protection_as_raw_text(self):
        jsonl = (f'{{"cwd": "\\/Users\\/{USER}\\/w"}}\n{{"note": "a b", "repo": "example-private\\/secret-repo", '
                 f'"pr": 41}}\nnot json /Users/{USER}/z\n')
        self.stage({
            "escaped.json": (f'{{"a": {{"repo": "example-private\\/secret-repo"}}, "b": {{"path": '
                             f'"\\u002fUsers\\u002f{USER}\\u002fnotes", "log": "cwd:\\n/Users/{USER}/work/a.txt", '
                             f'"tab": "a\\t{HOME}/x.txt"}}}}\n'),
            "keys.json": {"repos": {PRIVATE: {"pr": 41}}, "about": "unrelated",
                          "paths": {f"/Users/{USER}/a": 1, "~/a": 2}},
            "failed.py": (f'MSG = "failed\\n/Users/{USER}/work/a.txt\\t{HOME}/b.txt"\nURL = "\\/Users\\/{USER}\\/work"\n'
                          f'REF = "see\\n{PRIVATE}#3\\nhttps://github.com/{PRIVATE}/issues/4"\n'),
            "lines.jsonl": jsonl,
        })
        self.guard("staged")
        self.assertEqual(self.staged_json("escaped.json"), {
            "a": {"repo": "example-org/example-repo-1"},
            "b": {"path": "~/notes", "log": "cwd:\n~/work/a.txt", "tab": "a\t~/x.txt"}})
        self.assertEqual(self.staged_json("keys.json"), {
            "repos": {"example-org/example-repo-1": {"pr": 1}}, "about": "unrelated",
            "paths": {"~/a (2)": 1, "~/a": 2}})
        self.assertEqual(self.staged("failed.py"), 'MSG = "failed\\n~/work/a.txt\\t~/b.txt"\nURL = "~\\/work"\n'
                         'REF = "see\\nan issue or pull request in a private repository\\nan issue in a private repository"\n')
        self.assertEqual(self.staged("lines.jsonl"), '{"cwd": "~/w"}\n{"note": "a b", "repo": '
                                                     '"example-org/example-repo-1", "pr": 1}\nnot json ~/z\n')

    def test_every_github_reference_spelling_is_recognised(self):
        self.stage({"notes.md": textwrap.dedent(f"""\
            Clone git@github.com:{PRIVATE}.git or ssh://git@github.com/{PRIVATE}.git.
            See https://GITHUB.COM/{PRIVATE}/pull/7 and https://api.github.com/repos/{PRIVATE}/pulls/41.
            Raw: https://raw.githubusercontent.com/{PRIVATE}/main/README.md
            Gists: https://gist.github.com/{USER}/{SECRET_GIST}, https://gist.github.com/{PUBLIC_GIST}
            Keep git@github.com:{PUBLIC}.git.
            """),
            "remotes.json": {"remote": f"git@github.com:{PRIVATE}.git",
                             "api": f"https://api.github.com/repos/{PRIVATE}/pulls/41",
                             "gist": f"https://gist.github.com/{USER}/{SECRET_GIST}"}})
        output = self.guard("staged")
        self.assertEqual(self.staged("notes.md"), textwrap.dedent(f"""\
            Clone a private repository or a private repository.
            See a pull request in a private repository and a pull request in a private repository.
            Raw: a private repository
            Gists: a private gist, https://gist.github.com/{PUBLIC_GIST}
            Keep git@github.com:{PUBLIC}.git.
            """))
        self.assertEqual(self.staged_json("remotes.json"), {
            "remote": "git@github.com:example-org/example-repo-1.git",
            "api": "https://api.github.com/repos/example-org/example-repo-1/pulls/1",
            "gist": "https://gist.github.com/example-org/example-gist-1"})
        self.assertIn("placeholder example-org/example-gist-1 stands for a private gist", output)

    def test_autolinks_are_replaced_whole(self):
        self.stage({"notes.md": f"See <https://github.com/{PRIVATE}/pull/3> and <https://github.com/{PUBLIC}/pull/3>.\n"})
        self.guard("staged")
        self.assertEqual(self.staged("notes.md"),
                         f"See a pull request in a private repository and <https://github.com/{PUBLIC}/pull/3>.\n")

    def test_url_form_of_an_unknown_repository_counts_as_private_when_it_is_also_bare(self):
        self.stage({"notes.md": f"Moved {GONE} out; history at https://github.com/{GONE}\n"})
        self.guard("staged")
        self.assertEqual(self.staged("notes.md"), "Moved a private repository out; history at a private repository\n")

    def test_path_spellings_and_file_uris_are_recognised(self):
        paths = publication_guard.LocalPaths(f"{HOME}/src/example-app", [], [], [HOME], USER)
        self.assertEqual(paths.rewrite(
            f"/Users//{USER}/a, /Users/./{USER}/b, {HOME}//src/./example-app/c.py, "
            f"[d](file://{HOME}/src/example-app/d.py), file:///Users/{USER}/e, file://localhost{HOME}/f"),
            ("~/a, ~/b, ./c.py, [d](./d.py), ~/e, ~/f", 6))

    def test_single_segment_home_keeps_command_line_flags(self):
        paths = publication_guard.LocalPaths(homes=["/root"])
        self.assertEqual(paths.rewrite("Run `tool -root` and `find . -root-dir x`; cache in /root/.cache/a"),
                         ("Run `tool -root` and `find . -root-dir x`; cache in ~/.cache/a", 1))
        deeper = publication_guard.LocalPaths(homes=[HOME])
        self.assertEqual(deeper.rewrite("-srv-example-home-src-app/log"), ("example-project/log", 1))

    def test_unreadable_files_are_named_and_bom_marked_text_is_decoded(self):
        wide = codecs.BOM_UTF16_LE + f"Ported from {PRIVATE} in /Users/{USER}/x\r\n".encode("utf-16-le")
        self.stage({"wide.txt": wide, "nul.md": f"Ported from {PRIVATE}\0 end\n".encode(),
                    "legacy.txt": f"Caf\xe9 notes from {PRIVATE}\n".encode("latin-1")})
        output = self.guard("staged")
        self.assertEqual(self.staged_bytes("wide.txt"),
                         codecs.BOM_UTF16_LE + "Ported from a private repository in ~/x\r\n".encode("utf-16-le"))
        self.assertEqual(self.staged_bytes("nul.md"), b"Ported from a private repository\0 end\n")
        self.assertIn("warning: legacy.txt was not checked", output)
        self.git("commit", "-q", "-m", "fixtures")
        self.stage({"image.png": b"\x89PNG\r\n\x1a\n\x00\x00"})
        output = self.guard("staged")
        self.assertIn("nothing to rewrite in the files it could read", output)
        self.assertIn("warning: image.png was not checked", output)

    def test_public_repository_inside_private_data_keeps_its_numbers(self):
        self.stage({"a.json": {"repo": PRIVATE, "pr": 41,
                               "upstream": {"repo": PUBLIC, "pr": 41, "head": SHA, "short": SHA[:7]}}})
        self.guard("staged")
        self.assertEqual(self.staged_json("a.json"), {
            "repo": "example-org/example-repo-1", "pr": 1,
            "upstream": {"repo": PUBLIC, "pr": 41, "head": SHA, "short": SHA[:7]}})

    def test_github_errors_stop_the_guard_without_changes(self):
        text = "#include <sys/types.h>\nUse and/or here, or example-private/deleted-repo.\n"
        self.stage({"notes.md": text, "a.json": {"note": "read/write mode", "n": 1}})
        before = (self.staged("notes.md"), self.staged("a.json"))
        for fail in ("lookup", "user", "orgs"):
            result = self.run_guard("staged", env={"FAKE_GH_FAIL": fail})
            self.assertEqual(result.returncode, 2, fail)
            self.assertIn("cannot ask GitHub", result.stderr)
            self.assertEqual((self.staged("notes.md"), self.staged("a.json")), before)
            self.assertEqual((self.repo / "notes.md").read_text(), text)
        self.stage({"flaky.md": f"See https://github.com/{FLAKY}\n"})
        self.assertEqual(self.run_guard("staged").returncode, 2)
        self.assertFalse(list((self.repo / ".git").glob("kgr-publication-guard.json*")))
        latin = self.base / "latin"
        latin.write_bytes(f"fix: caf\xe9 /Users/{USER}/x\n".encode("latin-1"))
        for message in (self.base / "missing", latin):
            result = self.run_guard("staged", "--message-file", str(message))
            self.assertEqual(result.returncode, 2)
            self.assertIn("as UTF-8 text", result.stderr)
        self.assertEqual(latin.read_bytes(), f"fix: caf\xe9 /Users/{USER}/x\n".encode("latin-1"))

    def test_merge_of_the_base_checks_only_what_the_merge_changes(self):
        self.stage({"c.md": "base\n"})
        self.git("commit", "-q", "-m", "c")
        self.git("checkout", "-q", "-b", "feature")
        self.stage({"c.md": "feature side\n"})
        self.git("commit", "-q", "-m", "feature edit")
        self.git("checkout", "-q", "main")
        howto = f"Clone https://github.com/{GONE} and run it.\n"
        self.stage({"c.md": "main side\n", "docs/howto.md": howto})
        self.git("commit", "-q", "-m", "main edit")
        self.git("checkout", "-q", "feature")
        subprocess.run(["git", "merge", "main"], cwd=self.repo, capture_output=True, env=self.env)
        self.stage({"c.md": f"resolved from {PRIVATE}\n"})
        self.guard("staged")
        self.assertEqual(self.staged("docs/howto.md"), howto)
        self.assertEqual(self.staged("c.md"), "resolved from a private repository\n")

    def test_line_endings_are_kept_and_each_change_is_counted_once(self):
        self.stage({"win.md": f"line one\r\nPorted from {PRIVATE}.\r\nline three\r\n".encode(),
                    "win.json": f'{{\r\n  "repo": "{PRIVATE}",\r\n  "pr": 1\r\n}}\r\n'.encode()})
        output = self.guard("staged")
        self.assertIn("rewrote win.md (staged): references 1\n", output)
        self.assertEqual(self.staged_bytes("win.md"), b"line one\r\nPorted from a private repository.\r\nline three\r\n")
        self.assertEqual((self.repo / "win.md").read_bytes(), self.staged_bytes("win.md"))
        self.assertIn("rewrote win.json (staged): references 1\n", output)
        self.assertEqual(self.staged_bytes("win.json"), b'{\r\n  "repo": "example-org/example-repo-1",\r\n  "pr": 1\r\n}\r\n')
        self.assertEqual(self.git("status", "--porcelain"), "A  win.json\nA  win.md\n")

    def test_unstaged_edits_are_kept(self):
        self.stage({"notes.md": f"Ported from {PRIVATE}.\n"})
        (self.repo / "notes.md").write_text(f"Ported from {PRIVATE}.\nA line still being written.\n")
        self.assertIn("rewrote notes.md (staged): references 1\n", self.guard("staged"))
        self.assertEqual(self.staged("notes.md"), "Ported from a private repository.\n")
        self.assertEqual((self.repo / "notes.md").read_text(),
                         "Ported from a private repository.\nA line still being written.\n")

    def test_a_symlink_target_is_never_written(self):
        outside = self.base / "outside.md"
        outside.write_text(f"Kept in {PRIVATE}.\n")
        (self.repo / "link.md").symlink_to(outside)
        self.stage({"notes.md": f"Ported from {PRIVATE}.\n"})
        self.git("add", "link.md")
        self.guard("staged")
        self.assertEqual(outside.read_text(), f"Kept in {PRIVATE}.\n")
        self.assertTrue((self.repo / "link.md").is_symlink())

    def test_abbreviated_shas_stay_prefixes_of_their_full_form_in_any_order(self):
        self.stage({"a.json": {"repo": PRIVATE, "seven": SHA[:7], "ten": SHA[:10], "full": SHA}})
        self.guard("staged")
        mapped = self.staged_json("a.json")
        self.assertNotEqual(mapped["full"], SHA)
        self.assertEqual(mapped["seven"], mapped["full"][:7])
        self.assertEqual(mapped["ten"], mapped["full"][:10])

    def test_history_names_unusual_file_names_escaped_json_unreadable_files_and_merge_commits(self):
        start = self.git("rev-parse", "HEAD").strip()
        names = ["sp ace.md", 'q"uote.md', "tab\tname.md", "caf\u00e9.md"]
        self.stage({**{name: f"Read /Users/{USER}/notes.txt\n" for name in names},
                    "escaped.json": '{"repo": "example-private\\/secret-repo"}\n',
                    "legacy.txt": f"Caf\xe9 notes from {PRIVATE}\n".encode("latin-1")})
        self.git("commit", "-q", "-m", "docs: add notes")
        leaked = self.git("rev-parse", "--short=7", "HEAD").strip()
        self.git("rm", "-q", *names, "escaped.json", "legacy.txt")
        self.git("commit", "-q", "-m", "docs: drop notes")
        self.git("checkout", "-q", "-b", "side")
        self.stage({"c.md": f"side at /Users/{USER}/side\n", "e.md": "side e\n"})
        self.git("commit", "-q", "-m", "side edit")
        side = self.git("rev-parse", "--short=7", "HEAD").strip()
        self.git("checkout", "-q", "main")
        self.stage({"c.md": "main\n", "e.md": "main e\n"})
        self.git("commit", "-q", "-m", "main edit")
        subprocess.run(["git", "merge", "side"], cwd=self.repo, capture_output=True, env=self.env)
        # c.md only combines lines its parents already have; e.md gains a new one.
        self.stage({"c.md": f"side at /Users/{USER}/side\nmain\n", "e.md": f"resolved at /Users/{USER}/x\n"})
        self.git("commit", "-q", "-m", f"Merge the port from https://github.com/{GONE}/pull/2")
        merge = self.git("rev-parse", "--short=7", "HEAD").strip()
        output = self.guard("outgoing", "--base", start)
        self.assertIn(f"commit {leaked} adds a private reference or local path to caf\u00e9.md, escaped.json, "
                      f'q"uote.md, sp ace.md, tab\tname.md; pushing publishes that commit unchanged', output)
        self.assertIn(f"commit {side} adds a private reference or local path to c.md;", output)
        self.assertIn(f"commit {merge} adds a private reference or local path to e.md, its message;", output)
        self.assertEqual(output.count("adds a private reference"), 3)
        self.assertIn("warning: legacy.txt was not checked", output)

    def test_parallel_runs_never_hand_out_one_placeholder_twice(self):
        state = self.base / "kgr-publication-guard.json"
        (self.base / "kgr-publication-guard.json.tmp").mkdir()  # A stale fixed-name temporary file.
        entered, mapped = threading.Event(), {}

        def other_lane():
            with publication_guard.Mapper(state) as mapper:
                entered.set()
                mapped["beta"] = mapper.repo("example-private/beta")

        with publication_guard.Mapper(state) as mapper:
            mapped["alpha"] = mapper.repo("example-private/alpha")
            lane = threading.Thread(target=other_lane)
            lane.start()
            self.assertFalse(entered.wait(0.5))
        lane.join(10)
        self.assertEqual(mapped, {"alpha": "example-org/example-repo-1", "beta": "example-org/example-repo-2"})
        self.assertEqual(json.loads(state.read_text())["repos"], {"example-private/alpha": "example-org/example-repo-1",
                                                                  "example-private/beta": "example-org/example-repo-2"})

    def test_unreadable_placeholder_map_stops_the_guard(self):
        (self.repo / ".git/kgr-publication-guard.json").write_text("{not json")
        self.stage({"notes.md": f"Ported from {PRIVATE}.\n"})
        result = self.run_guard("staged")
        self.assertEqual(result.returncode, 2)
        self.assertIn("cannot read the placeholder map", result.stderr)
        self.assertEqual(self.staged("notes.md"), f"Ported from {PRIVATE}.\n")

    def test_staged_content_is_written_back_without_running_filters_again(self):
        self.git("config", "filter.quote.clean", "sed 's/^/> /'")
        (self.repo / ".gitattributes").write_text("quoted.md filter=quote\n")
        self.stage({"quoted.md": f"Ported from {PRIVATE}.\n"})
        self.assertEqual(self.staged("quoted.md"), f"> Ported from {PRIVATE}.\n")
        self.guard("staged")
        self.assertEqual(self.staged("quoted.md"), "> Ported from a private repository.\n")

    def test_long_unbroken_lines_are_scanned_in_linear_time(self):
        started = time.monotonic()
        self.assertEqual(list(publication_guard.REFERENCE.finditer("+" * 40000)), [])
        self.assertLess(time.monotonic() - started, 5)

    def test_only_lines_the_branch_adds_are_rewritten(self):
        notes = f"Clone https://github.com/{GONE} first.\nNotes in /Users/{USER}/notes.txt\n"
        body = f'    "upstream" : "{GONE}",\n    "log" : "/Users/{USER}/a",\n    "pr" : 12\n}}\n'
        self.stage({"notes.md": notes, "a.json": "{\n" + body})
        self.git("commit", "-q", "-m", "base")
        base = self.git("rev-parse", "HEAD").strip()
        self.stage({"notes.md": notes + f"Track {GONE}#12 too.\n", "a.json": f'{{\n    "ref" : "{GONE}#12",\n' + body})
        self.git("commit", "-q", "-m", "branch")
        self.guard("outgoing", "--base", base)
        fixed = notes + "Track an issue or pull request in a private repository too.\n"
        self.assertEqual(self.staged("notes.md"), fixed)
        self.assertEqual(self.staged("a.json"), '{\n    "ref" : "example-org/example-repo-1#1",\n' + body)
        self.stage({"notes.md": fixed + f"Also /Users/{USER}/new.txt\n"})
        self.guard("staged")
        self.assertEqual(self.staged("notes.md"), fixed + "Also ~/new.txt\n")

if __name__ == "__main__":
    unittest.main()
