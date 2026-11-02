#!/usr/bin/env python3
"""The binding-guides listing finds every guide that governs a change."""

from __future__ import annotations

import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


SCRIPT = Path(__file__).resolve().parents[1] / "scripts" / "binding_guides.py"


def run(root: Path, *args: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [sys.executable, str(SCRIPT), *args],
        cwd=root,
        capture_output=True,
        text=True,
        check=False,
    )


def listed(output: str) -> list[str]:
    return [line for line in output.splitlines() if line and not line.startswith(" ")]


class BindingGuidesTests(unittest.TestCase):
    def setUp(self) -> None:
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.root = Path(self.tmp.name)
        self.git("init", "-q", "-b", "main")
        self.git("config", "user.email", "test@example.com")
        self.git("config", "user.name", "Test")
        self.write("README.md", "# Project\n")
        self.git("add", ".")
        self.git("commit", "-q", "-m", "initial")
        self.base = self.git("rev-parse", "HEAD").strip()

    def git(self, *args: str) -> str:
        return subprocess.run(
            ["git", *args], cwd=self.root, capture_output=True, text=True, check=True
        ).stdout

    def write(self, path: str, text: str) -> None:
        target = self.root / path
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(text, encoding="utf-8")

    def commit_guides(self, files: dict[str, str]) -> None:
        for path, text in files.items():
            self.write(path, text)
        self.git("add", ".")
        self.git("commit", "-q", "-m", "guides")
        self.base = self.git("rev-parse", "HEAD").strip()

    def test_lists_guides_that_govern_changed_paths_only(self) -> None:
        self.commit_guides(
            {
                "AGENTS.md": "# Root\n",
                "app/AGENTS.md": "# App\n",
                "web/AGENTS.md": "# Web\n",
                "app/src/main.ts": "export {};\n",
            }
        )
        self.write("app/src/main.ts", "export const a = 1;\n")

        result = run(self.root, "--base", self.base)

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(listed(result.stdout), ["AGENTS.md", "app/AGENTS.md"])
        self.assertIn("governs app/src/main.ts", result.stdout)

    def test_follows_links_to_the_guides_an_instruction_file_names(self) -> None:
        self.commit_guides(
            {
                "AGENTS.md": (
                    "Follow [testing](docs/testing.md) and the\n"
                    "[comment rules](docs/code-style.md#comments).\n"
                    "See [the site](https://example.com/guide.md) and [a missing doc](docs/gone.md).\n"
                    "```md\n[example only](docs/example.md)\n```\n"
                ),
                "docs/testing.md": "Use [fixtures][f].\n\n[f]: fixtures/README.md\n",
                "docs/fixtures/README.md": "Fixture rules link to [deeper](deeper.md).\n",
                "docs/fixtures/deeper.md": "# Too deep\n",
                "docs/code-style.md": "## Comments\n",
                "docs/example.md": "# Example\n",
                "src/lib.py": "",
            }
        )
        self.write("src/lib.py", "VALUE = 1\n")

        result = run(self.root, "--base", self.base)

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(
            listed(result.stdout), ["AGENTS.md", "docs/testing.md", "docs/code-style.md"]
        )
        self.assertIn("linked from AGENTS.md:2 (#comments)", result.stdout)

        deeper = run(self.root, "--base", self.base, "--depth", "2")
        self.assertEqual(
            listed(deeper.stdout)[3:], ["docs/fixtures/README.md"]
        )
        self.assertIn("linked from docs/testing.md:3", deeper.stdout)

    def test_reads_a_claude_import_and_the_contribution_guide(self) -> None:
        self.commit_guides(
            {
                "CLAUDE.md": "@AGENTS.md\n\nClaude-specific additions.\n",
                "AGENTS.md": "Write tests as in [the guide](docs/testing.md).\n",
                "docs/testing.md": "# Testing\n",
                ".github/CONTRIBUTING.md": "# Contributing\n",
            }
        )
        self.write("new_module.py", "VALUE = 1\n")

        result = run(self.root, "--base", self.base)

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(
            sorted(listed(result.stdout)),
            [".github/CONTRIBUTING.md", "AGENTS.md", "CLAUDE.md", "docs/testing.md"],
        )
        self.assertIn("governs new_module.py", result.stdout)
        self.assertIn("contribution guide", result.stdout)

    def test_lists_an_instruction_file_linked_from_outside_the_checkout(self) -> None:
        with tempfile.TemporaryDirectory() as shared:
            outside = Path(shared) / "AGENTS.md"
            outside.write_text("Follow [testing](docs/testing.md).\n", encoding="utf-8")
            (self.root / "AGENTS.md").symlink_to(outside)
            self.write("docs/testing.md", "# Testing\n")
            self.write("main.py", "VALUE = 1\n")

            result = run(self.root, "--base", self.base)

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(listed(result.stdout), ["AGENTS.md", "docs/testing.md"])
        self.assertIn("linked from AGENTS.md:1", result.stdout)

    def test_reports_when_no_guide_exists(self) -> None:
        self.write("README.md", "# Changed\n")

        result = run(self.root, "--base", self.base)

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout.strip(), "no binding guides found")

    def test_an_unknown_base_is_an_error(self) -> None:
        result = run(self.root, "--base", "no-such-revision")

        self.assertEqual(result.returncode, 2)
        self.assertIn("error:", result.stderr)
        self.assertEqual(result.stdout, "")

    def test_outside_a_checkout_is_an_error(self) -> None:
        with tempfile.TemporaryDirectory() as outside:
            result = run(Path(outside), "--base", "HEAD")

        self.assertEqual(result.returncode, 2)
        self.assertIn("error:", result.stderr)


if __name__ == "__main__":
    unittest.main()
