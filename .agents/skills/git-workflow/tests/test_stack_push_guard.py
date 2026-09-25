import copy
import importlib.util
import json
import os
from pathlib import Path
import shlex
import shutil
import subprocess
import sys
import tempfile
import unittest

SKILL = Path(__file__).resolve().parents[1]
SPEC = importlib.util.spec_from_file_location("stack_push_guard", SKILL / "scripts/stack_push_guard.py")
guard = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(guard)


class ValidationTests(unittest.TestCase):
    def setUp(self):
        self.admission = {"directory": "/fixture", "remote": "upstream", "url": "/remote",
                          "refs": [{"ref": "refs/heads/feature/naïve-key", "source": "a" * 40, "expected": "b" * 40}]}
        row = self.admission["refs"][0]
        self.line = f'{row["ref"]} {row["source"]} {row["ref"]} {row["expected"]}\n'

    def test_nested_unicode_refs_and_sha256_are_not_fixture_specific(self):
        self.assertEqual(len(guard.validate_updates(self.admission, "upstream", "/remote", self.line)), 1)
        c = copy.deepcopy(self.admission)
        c["refs"][0].update(source="a" * 64, expected="0" * 64)
        self.assertEqual(guard.validate_admission(c), c)

    def test_bad_scope_or_object_cannot_be_admitted(self):
        for field, value in [("ref", "refs/tags/release"), ("ref", "refs/heads/.hidden"),
                             ("ref", "refs/heads/a..b"), ("ref", "refs/heads/a\nb"),
                             ("source", "0" * 40), ("source", "HEAD"), ("expected", "f" * 64)]:
            with self.subTest(field=field, value=value):
                c = copy.deepcopy(self.admission)
                c["refs"][0][field] = value
                with self.assertRaises(guard.GuardError):
                    guard.validate_admission(c)
        c = copy.deepcopy(self.admission)
        c["refs"] *= 2
        with self.assertRaises(guard.GuardError):
            guard.validate_admission(c)

    def test_corrupt_redirected_deleted_or_duplicated_updates_fail(self):
        for text in ["invalid\n", self.line * 2, self.line.replace("feature/naïve-key", "other", 1),
                     self.line.replace("a" * 40, "0" * 40), self.line.replace("b" * 40, "c" * 40)]:
            with self.subTest(text=text), self.assertRaises(guard.GuardError):
                guard.validate_updates(self.admission, "upstream", "/remote", text)
        for remote, url in [("origin", "/remote"), ("upstream", "/other")]:
            with self.assertRaises(guard.GuardError):
                guard.validate_updates(self.admission, remote, url, self.line)
        self.assertEqual(guard.validate_updates(self.admission, "upstream", "/remote", ""), [])

    def test_no_arbitrary_commands_or_hook_bypass_in_run(self):
        valid = ["gh", "stack", "submit", "--auto", "--remote", "upstream"]
        guard.validate_native_command(valid, self.admission)
        guard.validate_native_command(["gh", "stack", "push", "--remote", "upstream"], self.admission)
        guard.validate_native_command(["gh", "stack", "link", "12", "feature/naïve-key", "--remote", "upstream"], self.admission)
        for command in [valid + ["--no-verify"], ["sh", "-c", "anything"],
                        ["gh", "stack", "merge"], ["gh", "stack", "link", "12", "unapproved", "--remote", "upstream"],
                        valid[:-1] + ["other"], ["gh", "stack", "push", "--remote", "other"],
                        ["gh", "stack", "push", "--remote", "upstream", "--no-verify"]]:
            with self.subTest(command=command), self.assertRaises(guard.GuardError):
                guard.validate_native_command(command, self.admission)

    def test_duplicate_json_and_incomplete_config_fail(self):
        with self.assertRaises(guard.GuardError):
            guard.load_json('{"remote":"origin","remote":"elsewhere"}')
        for env in [{"GIT_CONFIG_COUNT": "01"}, {"GIT_CONFIG_COUNT": "1"}, {"GIT_CONFIG_COUNT": "-1"}]:
            with self.assertRaises(guard.GuardError):
                guard.config_count(env)


class StandaloneTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory(prefix="kgr-portable-guard-")
        self.addCleanup(self.temporary.cleanup)
        self.root = Path(self.temporary.name).resolve()
        # Only the skill is copied. Neither evals nor npm dependencies exist here.
        shutil.copytree(SKILL, self.root / "installed skill")
        self.helper = self.root / "installed skill/scripts/stack_push_guard.py"
        self.repo = self.root / "repository"
        self.remote = self.root / "remote.git"
        self.repo.mkdir()
        self.git("init", "--bare", "--initial-branch=main", str(self.remote))
        self.git("init", "--initial-branch=main")
        self.git("config", "user.name", "Fixture")
        self.git("config", "user.email", "fixture@example.invalid")
        self.git("commit", "--allow-empty", "-m", "initial")
        self.head = self.git("rev-parse", "HEAD")
        self.ref = "refs/heads/feature/portable-guard"
        self.git("branch", self.ref.removeprefix("refs/heads/"))
        self.git("remote", "add", "origin", str(self.remote))
        self.admission = {"directory": str(self.repo), "remote": "origin", "url": str(self.remote),
                          "refs": [{"ref": self.ref, "source": self.head, "expected": "0" * 40}]}

    def git(self, *args, environment=None, check=True):
        result = subprocess.run(["git", *args], cwd=self.repo, env=environment, capture_output=True, text=True)
        if check and result.returncode:
            self.fail(result.stderr)
        return result.stdout.strip() if check else result

    def prepare(self, environment=None, admission=None):
        result = subprocess.run([sys.executable, str(self.helper), "prepare", "--admission", "-"],
                                input=json.dumps(admission or self.admission), capture_output=True, text=True, env=environment)
        self.assertEqual(result.returncode, 0, result.stderr)
        return json.loads(result.stdout)

    def push(self, prepared, environment=None):
        return self.git("push", "origin", f"{self.ref}:{self.ref}",
                        environment={**(environment or os.environ), **prepared["configuration"]}, check=False)

    def test_standalone_copy_pushes_without_bun_or_repository_modules(self):
        prepared = self.prepare()
        result = self.push(prepared)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.git("ls-remote", "--heads", "origin", self.ref), f"{self.head}\t{self.ref}")
        receipt = json.loads(next(Path(prepared["directory"]).glob("check-*.json")).read_text())
        self.assertTrue(receipt["accepted"])
        self.assertEqual(receipt["updates"][0]["source"], self.head)

    def test_actual_sha256_and_unicode_ref_with_a_non_origin_remote(self):
        self.repo = self.root / "sha256 repository"
        self.remote = self.root / "sha256 remote.git"
        self.repo.mkdir()
        self.git("init", "--bare", "--object-format=sha256", "--initial-branch=main", str(self.remote))
        self.git("init", "--object-format=sha256", "--initial-branch=main")
        self.git("config", "user.name", "Fixture")
        self.git("config", "user.email", "fixture@example.invalid")
        self.git("commit", "--allow-empty", "-m", "sha256 fixture")
        self.head = self.git("rev-parse", "HEAD")
        self.ref = "refs/heads/feature/naïve-\u0085-key"
        self.git("branch", self.ref.removeprefix("refs/heads/"))
        self.git("remote", "add", "upstream", str(self.remote))
        self.admission = {"directory": str(self.repo), "remote": "upstream", "url": str(self.remote),
                          "refs": [{"ref": self.ref, "source": self.head, "expected": "0" * 64}]}
        prepared = self.prepare()
        result = self.git("push", "upstream", f"{self.ref}:{self.ref}",
                          environment={**os.environ, **prepared["configuration"]}, check=False)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(len(self.head), 64)
        self.assertEqual(self.git("ls-remote", "--heads", "upstream", self.ref), f"{self.head}\t{self.ref}")

    def test_preserves_existing_hook_environment_and_forwards_other_hooks(self):
        # Keep the configured hook directory in Git metadata so it is not dirty.
        hooks = self.repo / ".git" / "custom hooks"
        hooks.mkdir()
        self.git("config", "core.hooksPath", str(hooks))
        observed = self.root / "observed-hook-config"
        other = self.root / "other-hook"
        pre = hooks / "pre-push"
        pre.write_text(f"#!/bin/sh\ngit config --get test.retained > {shlex.quote(str(observed))}\ngit config --get core.hooksPath >> {shlex.quote(str(observed))}\ncat >/dev/null\n")
        pre.chmod(0o755)
        commit_hook = hooks / "pre-commit"
        commit_hook.write_text(f"#!/bin/sh\nprintf yes > {shlex.quote(str(other))}\n")
        commit_hook.chmod(0o755)
        env = {**os.environ, "GIT_CONFIG_COUNT": "1", "GIT_CONFIG_KEY_0": "test.retained", "GIT_CONFIG_VALUE_0": "retained"}
        prepared = self.prepare(env)
        result = self.push(prepared, env)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(observed.read_text(), f"retained\n{hooks}\n")
        self.git("hook", "run", "pre-commit", environment={**env, **prepared["configuration"]})
        self.assertEqual(other.read_text(), "yes")
        self.assertEqual(self.git("config", "core.hooksPath"), str(hooks))
        self.assertNotIn("GIT_CONFIG_VALUE_0", prepared["configuration"])

    def test_frozen_helper_change_rejects_before_remote_update(self):
        prepared = self.prepare()
        frozen = Path(prepared["directory"]) / "guard.py"
        frozen.write_text(frozen.read_text() + "\n# changed after admission\n")
        result = self.push(prepared)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("frozen guard configuration or runtime changed", result.stderr)
        self.assertEqual(self.git("ls-remote", "--heads", "origin", self.ref), "")

    def test_changed_admission_and_symlink_replacement_are_rejected(self):
        for mode in ["content", "symlink"]:
            prepared = self.prepare()
            config = Path(prepared["directory"]) / "admission.json"
            value = json.loads(config.read_text())
            if mode == "content":
                value["request"]["admission"]["remote"] = "elsewhere"
                config.write_text(json.dumps(value))
            else:
                target = config.with_suffix(".copy")
                target.write_text(config.read_text())
                config.unlink()
                config.symlink_to(target)
            result = self.push(prepared)
            self.assertNotEqual(result.returncode, 0)
            self.assertEqual(self.git("ls-remote", "--heads", "origin", self.ref), "")

    def test_dirty_checkout_and_changed_push_url_do_not_prepare(self):
        for mode in ["dirty", "push-url"]:
            if mode == "dirty":
                (self.repo / "unrelated").write_text("user work")
            else:
                (self.repo / "unrelated").unlink()
                self.git("remote", "set-url", "--push", "origin", str(self.root / "another.git"))
            result = subprocess.run([sys.executable, str(self.helper), "prepare", "--admission", "-"],
                                    input=json.dumps(self.admission), capture_output=True, text=True)
            self.assertNotEqual(result.returncode, 0)
            self.assertFalse((self.repo / ".git/kgr-push-guards").exists())

    def test_run_cli_preserves_guard_and_child_exit_without_an_sdk(self):
        # Native argv routing is tested here with a tiny gh stand-in; live tests
        # separately exercise the installed official extension against GitHub.
        gh = self.root / "gh"
        gh.write_text(f'#!/bin/sh\nif [ "$2" = "--version" ]; then printf "gh stack version 0.1.0\\n"; exit 0; fi\nexec git push origin {shlex.quote(self.ref + ":" + self.ref)}\n')
        gh.chmod(0o755)
        result = subprocess.run([sys.executable, str(self.helper), "run", "--admission", "-", "--", str(gh),
                                 "stack", "submit", "--auto", "--remote", "origin"],
                                input=json.dumps(self.admission), capture_output=True, text=True)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.git("ls-remote", "--heads", "origin", self.ref), f"{self.head}\t{self.ref}")
        receipts = list((self.repo / ".git/kgr-push-guards").glob("*/run.json"))
        self.assertEqual(len(receipts), 1)
        self.assertEqual(json.loads(receipts[0].read_text())["exitCode"], 0)


if __name__ == "__main__":
    unittest.main()
