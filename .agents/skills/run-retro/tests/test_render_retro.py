import importlib.util
import json
import unittest
from pathlib import Path


SCRIPT_PATH = Path(__file__).parents[1] / "scripts" / "render_retro.py"
SPEC = importlib.util.spec_from_file_location("render_retro", SCRIPT_PATH)
assert SPEC and SPEC.loader
RENDER_RETRO = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(RENDER_RETRO)


class RenderRetroTests(unittest.TestCase):
    def test_renders_interactive_states_deep_dive_and_diagram(self):
        fixture = Path(__file__).with_name("interactive-fixture.json")
        data = json.loads(fixture.read_text(encoding="utf-8"))
        data["changes"][0]["deepDiveUrl"] = "codex:thread/example"

        rendered = RENDER_RETRO.render_report(data, Path.cwd())

        self.assertIn("Before", rendered)
        self.assertIn("After", rendered)
        self.assertIn("Deep dive", rendered)
        self.assertIn("language-typescript", rendered)
        self.assertIn("Invalid input now returns a typed result", rendered)
        self.assertIn('data-state-view="before"', rendered)
        self.assertIn("Copy deep-dive prompt", rendered)
        self.assertIn('href="codex:thread/example"', rendered)
        self.assertIn('data-diagram-action="play"', rendered)
        self.assertIn("Validated input flow", rendered)

    def test_rejects_impact_over_300_characters(self):
        data = {
            "title": "Impact report",
            "changes": [
                {
                    "title": "Too long",
                    "impact": "x" * 301,
                    "before": {"kind": "text", "content": "Before"},
                    "after": {"kind": "text", "content": "After"},
                }
            ],
        }

        with self.assertRaisesRegex(ValueError, "exceeds 300 characters"):
            RENDER_RETRO.render_report(data, Path.cwd())

    def test_rejects_unsafe_deep_dive_url(self):
        data = {
            "title": "Impact report",
            "changes": [
                {
                    "title": "Unsafe link",
                    "impact": "The report rejects executable links.",
                    "before": {"kind": "text", "content": "Before"},
                    "after": {"kind": "text", "content": "After"},
                    "deepDiveUrl": "javascript:alert(1)",
                }
            ],
        }

        with self.assertRaisesRegex(ValueError, "must use codex, https, or http"):
            RENDER_RETRO.render_report(data, Path.cwd())

    def test_rejects_diagram_with_fewer_than_three_nodes(self):
        data = {
            "title": "Impact report",
            "changes": [
                {
                    "title": "Small diagram",
                    "impact": "The report validates meaningful diagrams.",
                    "before": {"kind": "text", "content": "Before"},
                    "after": {"kind": "text", "content": "After"},
                    "diagram": {
                        "title": "Too small",
                        "nodes": [
                            {"id": "one", "label": "One"},
                            {"id": "two", "label": "Two"},
                        ],
                        "edges": [],
                        "steps": [{"label": "One", "highlights": ["one"]}],
                    },
                }
            ],
        }

        with self.assertRaisesRegex(ValueError, "at least three"):
            RENDER_RETRO.render_report(data, Path.cwd())


if __name__ == "__main__":
    unittest.main()
