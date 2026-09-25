#!/usr/bin/env python3
"""Check agent-facing Markdown for the repository writing contract."""

from __future__ import annotations

import argparse
import re
from pathlib import Path
from typing import Iterable


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


def markdown_paths(root: Path) -> list[Path]:
    paths = [root / "README.md"]
    for skill in sorted(root.iterdir()):
        if skill.is_dir() and (skill / "SKILL.md").is_file():
            paths.extend(sorted(skill.rglob("*.md")))
    return paths


def check_lines(path: Path, lines: Iterable[str]) -> list[str]:
    findings: list[str] = []
    in_fence = False
    for line_number, line in enumerate(lines, start=1):
        stripped = line.lstrip()
        if stripped.startswith("```") or stripped.startswith("~~~"):
            in_fence = not in_fence
            continue
        if in_fence or stripped.startswith(">"):
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
    root = Path(__file__).resolve().parents[2]
    paths = args.paths or markdown_paths(root)
    findings = check_paths(path.resolve() for path in paths)
    if findings:
        raise SystemExit("\n".join(findings))
    print(f"Checked {len(paths)} Markdown files")


if __name__ == "__main__":
    main()
