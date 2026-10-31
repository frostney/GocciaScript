#!/usr/bin/env python3
"""Check agent-facing Markdown for the repository writing contract."""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path
from typing import Iterable

sys.dont_write_bytecode = True  # Keep installed skill trees free of __pycache__.


BANNED_WORDS = re.compile(
    r"\b(?:seam|seams|honest|honestly|substrate|substrates)\b", re.IGNORECASE
)
BANNED_PATTERNS = (
    (
        "banned opener",
        re.compile(
            r"^\s*(?:great question|absolutely|certainly|of course)\b",
            re.IGNORECASE,
        ),
    ),
    (
        "banned construction not just X, but Y",
        re.compile(r"\bnot\s+just\b[^.\n]{0,200}\bbut\b", re.IGNORECASE),
    ),
    (
        "banned closer",
        re.compile(
            r"\b(?:i hope this helps|let me know if|happy to help)\b",
            re.IGNORECASE,
        ),
    ),
)
INLINE_CODE = re.compile(r"`[^`]*`")
FENCE = re.compile(r"(`{3,}|~{3,})(.*)")
SKILL_DIR = Path(__file__).resolve().parents[1]


def suite_skills(root: Path) -> set[str] | None:
    """Skills installed from this suite's source, or None without a usable lock.

    A project install keeps `skills-lock.json` beside `.agents/`, and its
    skills root can hold skills from other sources that this contract does not
    govern.
    """
    lock = root.parent.parent / "skills-lock.json"
    try:
        skills = json.loads(lock.read_text(encoding="utf-8"))["skills"]
        source = skills[SKILL_DIR.name]["source"]
    except (OSError, ValueError, KeyError, TypeError):
        return None
    if not isinstance(source, str) or not source:
        return None
    return {
        name
        for name, entry in skills.items()
        if isinstance(entry, dict) and entry.get("source") == source
    }


def markdown_paths(root: Path) -> list[Path]:
    suite = suite_skills(root)
    # In a project install, a README beside the skills is not this suite's.
    readme = root / "README.md"
    paths = [readme] if suite is None and readme.is_file() else []
    for skill in sorted(root.iterdir()):
        if suite is not None and skill.name not in suite:
            continue
        if skill.is_dir() and (skill / "SKILL.md").is_file():
            paths.extend(sorted(skill.rglob("*.md")))
    return paths


def check_lines(path: Path, lines: Iterable[str]) -> list[str]:
    findings: list[str] = []
    fence = ""  # The open code block's run of backticks or tildes.
    for line_number, line in enumerate(lines, start=1):
        stripped = line.lstrip()
        fence_match = FENCE.match(stripped)
        marker, rest = fence_match.groups() if fence_match else ("", "")
        if fence:
            # CommonMark closes a block only on the character that opened it,
            # in a run at least as long, with nothing after it.
            if marker[:1] == fence[0] and len(marker) >= len(fence) and not rest.strip():
                fence = ""
            continue
        # A backtick fence cannot carry a backtick in its info string.
        if marker and not (marker[0] == "`" and "`" in rest):
            fence = marker
            continue
        if stripped.startswith(">"):
            continue

        prose = INLINE_CODE.sub("", line)
        if "—" in prose:
            findings.append(f"{path}:{line_number}: em dash")
        for match in BANNED_WORDS.finditer(prose):
            findings.append(
                f"{path}:{line_number}: banned word {match.group(0).lower()}"
            )
        for label, pattern in BANNED_PATTERNS:
            if pattern.search(prose):
                findings.append(f"{path}:{line_number}: {label}")
    return findings


def check_paths(paths: Iterable[Path]) -> list[str]:
    findings: list[str] = []
    for path in paths:
        findings.extend(check_lines(path, path.read_text(encoding="utf-8").splitlines()))
    return findings


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("paths", nargs="*", type=Path)
    args = parser.parse_args()
    root = SKILL_DIR.parent
    paths = args.paths or markdown_paths(root)
    if not paths:
        raise SystemExit(f"no Markdown files found under {root}")
    findings = check_paths(path.resolve() for path in paths)
    if findings:
        raise SystemExit("\n".join(findings))
    print(f"Checked {len(paths)} Markdown files")


if __name__ == "__main__":
    main()
