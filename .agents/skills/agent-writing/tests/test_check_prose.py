import importlib.util
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


if __name__ == "__main__":
    unittest.main()
