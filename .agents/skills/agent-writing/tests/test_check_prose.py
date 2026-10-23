import importlib.util
import json
import tempfile
import unittest
from pathlib import Path


SCRIPT_PATH = Path(__file__).parents[1] / "scripts" / "check_prose.py"
SPEC = importlib.util.spec_from_file_location("check_prose", SCRIPT_PATH)
assert SPEC and SPEC.loader
CHECK_PROSE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(CHECK_PROSE)


class CheckProseTests(unittest.TestCase):
    def test_flags_prose_but_ignores_code_and_quoted_source(self):
        findings = CHECK_PROSE.check_lines(
            Path("sample.md"),
            [
                "A prohibited—mark appears here.",
                "A banned seam appears here.",
                "Great question. The parser now passes.",
                "This is not just shorter, but clearer.",
                "The parser passes. Let me know if you want more detail.",
                "`seam` and `—` are code.",
                "`Great question` and `not just X, but Y` are code.",
                "> Quoted seam — remains exact.",
                "> Great question. Quoted source remains exact.",
                "```text",
                "seam — and Great question inside a fence",
                "```",
            ],
        )

        self.assertEqual(len(findings), 5)
        self.assertIn("em dash", findings[0])
        self.assertIn("banned word", findings[1])
        self.assertIn("banned opener", findings[2])
        self.assertIn("banned construction", findings[3])
        self.assertIn("banned closer", findings[4])

    def test_code_block_ends_only_at_its_own_closing_fence(self):
        code = "seam — inside the code block"
        for name, block, closed in (
            ("tilde block", ["~~~", code, "~~~"], True),
            ("tilde line inside a backtick block", ["```text", "~~~", code, "~~~", "```"], True),
            ("backtick line inside a tilde block", ["~~~text", "```", code, "```", "~~~"], True),
            ("backtick in a tilde block's info string", ["~~~ `text`", code, "~~~"], True),
            ("shorter fence inside a longer one", ["````markdown", "```", code, "```", "````"], True),
            ("closing fence longer than the opening one", ["```text", code, "````"], True),
            ("closing fence followed by spaces", ["```text", code, "```  "], True),
            ("closing fence followed by text", ["```text", "``` not a close", code, "```"], True),
            ("indented block in a list", ["- item", "", "  ```text", "  " + code, "  ```"], True),
            ("block left open at the end", ["```text", code], False),
        ):
            with self.subTest(name):
                findings = CHECK_PROSE.check_lines(
                    Path("sample.md"), [*block, "A banned seam after the block."]
                )
                self.assertEqual(
                    findings,
                    [f"sample.md:{len(block) + 1}: banned word seam"] if closed else [],
                )

    def test_line_that_cannot_open_a_code_block_is_checked_as_prose(self):
        findings = CHECK_PROSE.check_lines(
            Path("sample.md"),
            ["```inline``` code, then a banned seam.", "Prose with a banned seam."],
        )

        self.assertEqual(
            findings,
            ["sample.md:1: banned word seam", "sample.md:2: banned word seam"],
        )

    def test_opener_rule_only_applies_at_the_start_of_prose(self):
        findings = CHECK_PROSE.check_lines(
            Path("sample.md"),
            [
                "The result is absolutely stable.",
                "The command is certainly available.",
            ],
        )

        self.assertEqual(findings, [])

    def test_skill_markdown_passes(self):
        skill_root = Path(__file__).parents[1]
        findings = CHECK_PROSE.check_paths(sorted(skill_root.rglob("*.md")))

        self.assertEqual(findings, [])

    def test_skills_root_without_readme_checks_only_skill_markdown(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            skill_md = root / "sample-skill" / "SKILL.md"
            skill_md.parent.mkdir()
            skill_md.write_text("# Sample\n", encoding="utf-8")

            self.assertEqual(CHECK_PROSE.markdown_paths(root), [skill_md])

    def test_skills_root_readme_is_checked_first(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            readme = root / "README.md"
            readme.write_text("# Skills\n", encoding="utf-8")
            skill_md = root / "sample-skill" / "SKILL.md"
            skill_md.parent.mkdir()
            skill_md.write_text("# Sample\n", encoding="utf-8")

            self.assertEqual(CHECK_PROSE.markdown_paths(root), [readme, skill_md])

    def project_install(self, directory: str, sources: dict[str, str], lock: object = None) -> Path:
        project = Path(directory)
        root = project / ".agents" / "skills"
        for name in sources:
            (root / name).mkdir(parents=True)
            (root / name / "SKILL.md").write_text(f"# {name}\n", encoding="utf-8")
        if lock is None:
            lock = {
                "version": 1,
                "skills": {name: {"source": source} for name, source in sources.items()},
            }
        (project / "skills-lock.json").write_text(
            lock if isinstance(lock, str) else json.dumps(lock), encoding="utf-8"
        )
        return root

    def test_project_install_checks_only_skills_from_this_suite_source(self):
        with tempfile.TemporaryDirectory() as directory:
            root = self.project_install(directory, {
                "agent-writing": "owner/suite",
                "create-pr": "owner/suite",
                "improve-codebase-architecture": "someone/else",
            })
            (root / "README.md").write_text("# Project skills\n", encoding="utf-8")

            self.assertEqual(
                CHECK_PROSE.markdown_paths(root),
                [root / "agent-writing" / "SKILL.md", root / "create-pr" / "SKILL.md"],
            )

    def test_lock_without_this_skill_checks_every_skill(self):
        with tempfile.TemporaryDirectory() as directory:
            root = self.project_install(directory, {
                "create-pr": "owner/suite",
                "improve-codebase-architecture": "someone/else",
            })

            self.assertEqual(
                CHECK_PROSE.markdown_paths(root),
                [
                    root / "create-pr" / "SKILL.md",
                    root / "improve-codebase-architecture" / "SKILL.md",
                ],
            )

    def test_unusable_lock_checks_every_skill(self):
        sources = {"agent-writing": "owner/suite", "other": "", "third": "someone/else"}
        for lock in (
            "{not json",
            {"version": 1, "skills": []},
            {"version": 1, "skills": None},
            {"version": 1, "skills": {"agent-writing": "owner/suite"}},
            {"version": 1, "skills": {"agent-writing": {}}},
            {"version": 1, "skills": {
                "agent-writing": {"source": None}, "other": {}, "third": {"source": "someone/else"},
            }},
        ):
            with self.subTest(lock=lock), tempfile.TemporaryDirectory() as directory:
                root = self.project_install(directory, sources, lock)

                self.assertEqual(
                    CHECK_PROSE.markdown_paths(root),
                    [root / name / "SKILL.md" for name in sources],
                )


if __name__ == "__main__":
    unittest.main()
