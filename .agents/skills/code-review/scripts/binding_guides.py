#!/usr/bin/env python3
"""List the repository guides that bind a change.

Prints every `AGENTS.md` and `CLAUDE.md` that governs a changed path, the
contribution guide, and each Markdown file those link to, with the reason it
is listed. Exits 0 with the list, or with `no binding guides found`, and 2
when Git cannot answer or an argument is wrong.

Changed paths are the files that differ between BASE and the working tree,
plus untracked files that are not ignored.
"""

from __future__ import annotations

import argparse
import re
import subprocess
import sys
from pathlib import Path
from urllib.parse import unquote

sys.dont_write_bytecode = True  # Keep installed skill trees free of __pycache__.


INSTRUCTION_FILES = ("AGENTS.md", "CLAUDE.md")
# GitHub looks for a contribution guide in these directories.
CONTRIBUTING_DIRS = ("", "docs", ".github")
MARKDOWN_SUFFIXES = (".md", ".mdx", ".markdown")
FENCE = re.compile(r" {0,3}(`{3,}|~{3,})")
INLINE_LINK = re.compile(r"\]\(\s*<?([^)\s>]+)>?(?:\s+[\"'(][^)]*)?\)")
REFERENCE_LINK = re.compile(r" {0,3}\[[^\]]+\]:\s*<?([^\s>]+)>?")
# Claude Code's `@path` import, as in a CLAUDE.md that holds `@AGENTS.md`.
IMPORT = re.compile(r"(?:^|\s)@([\w./-]+)")
SCHEME = re.compile(r"[A-Za-z][A-Za-z0-9+.-]*:|//")


class GuideError(Exception):
    """A Git or argument problem that prevents the listing."""


def git(root: Path, *args: str) -> str:
    result = subprocess.run(
        ["git", *args], cwd=root, capture_output=True, text=True, check=False
    )
    if result.returncode != 0:
        raise GuideError(f"git {' '.join(args)}: {result.stderr.strip()}")
    return result.stdout


def changed_paths(root: Path, base: str) -> list[str]:
    git(root, "rev-parse", "--verify", "--quiet", f"{base}^{{commit}}")
    tracked = git(root, "diff", "--name-only", "--no-renames", base, "--")
    untracked = git(root, "ls-files", "--others", "--exclude-standard")
    return sorted({line for line in (tracked + untracked).splitlines() if line})


def instruction_files(root: Path, paths: list[str]) -> dict[Path, list[str]]:
    """Map each instruction file to the changed paths in its directory tree."""
    governed: dict[Path, list[str]] = {}
    for path in paths:
        directory = Path(path).parent
        for ancestor in (directory, *directory.parents):
            for name in INSTRUCTION_FILES:
                candidate = root / ancestor / name
                if candidate.is_file():
                    governed.setdefault(candidate, []).append(path)
    return governed


def contributing_guides(root: Path) -> list[Path]:
    found = []
    for directory in CONTRIBUTING_DIRS:
        folder = root / directory
        if folder.is_dir():
            found.extend(
                entry
                for entry in folder.iterdir()
                if entry.is_file() and entry.name.lower() == "contributing.md"
            )
    return sorted(found)


def link_targets(text: str) -> list[tuple[int, str]]:
    """Return (line number, target) for each link outside fenced code."""
    targets = []
    fence = None
    for number, line in enumerate(text.splitlines(), start=1):
        opening = FENCE.match(line)
        if opening:
            marker = opening.group(1)
            if fence is None:
                fence = marker
                continue
            if marker[0] == fence[0] and len(marker) >= len(fence):
                fence = None
                continue
        if fence is not None:
            continue
        found = INLINE_LINK.findall(line)
        found += [path.rstrip(".,;:") for path in IMPORT.findall(line)]
        reference = REFERENCE_LINK.match(line)
        if reference:
            found.append(reference.group(1))
        targets.extend((number, target) for target in found)
    return targets


def resolve(root: Path, source: Path, target: str) -> tuple[Path, str] | None:
    """Resolve a link to a Markdown file inside the repository, with its anchor."""
    if SCHEME.match(target) or target.startswith("#"):
        return None
    target, _, anchor = target.partition("#")
    target = unquote(target.partition("?")[0])
    base = root if target.startswith("/") else source.parent
    candidate = (base / target.lstrip("/")).resolve()
    if candidate.is_dir():
        candidate = candidate / "README.md"
    if not candidate.is_file() or candidate.suffix.lower() not in MARKDOWN_SUFFIXES:
        return None
    if not candidate.is_relative_to(root):
        return None
    return candidate, anchor


def list_guides(root: Path, base: str, depth: int) -> list[tuple[Path, list[str]]]:
    root = root.resolve()
    paths = changed_paths(root, base)
    reasons: dict[Path, list[str]] = {}
    governing = instruction_files(root, paths)
    # Root first, so a nested file reads as the narrower rule it is.
    for guide in sorted(governing, key=lambda path: (len(path.parts), path)):
        governed = governing[guide]
        shown = ", ".join(governed[:5])
        if len(governed) > 5:
            shown += f" and {len(governed) - 5} more"
        reasons.setdefault(guide, []).append(f"governs {shown}")
    for guide in contributing_guides(root):
        reasons.setdefault(guide, []).append("contribution guide")

    frontier = list(reasons)
    for _ in range(depth):
        # A guide lists only the links of the level that first reached it.
        links: dict[Path, dict[tuple[str, str], list[str]]] = {}
        for source in frontier:
            text = source.read_text(encoding="utf-8", errors="replace")
            name = source.relative_to(root).as_posix()
            for number, target in link_targets(text):
                resolved = resolve(root, source, target)
                if resolved is None or resolved[0] in reasons:
                    continue
                guide, anchor = resolved
                lines = links.setdefault(guide, {}).setdefault((name, anchor), [])
                if str(number) not in lines:
                    lines.append(str(number))
        for guide, sources in links.items():
            reasons[guide] = [
                f"linked from {name}:{','.join(lines)}" + (f" (#{anchor})" if anchor else "")
                for (name, anchor), lines in sources.items()
            ]
        frontier = list(links)
    return [(guide.relative_to(root), reasons[guide]) for guide in reasons]


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument(
        "--base", required=True, help="revision the change is compared with"
    )
    parser.add_argument("--root", type=Path, help="repository checkout (default: .)")
    parser.add_argument(
        "--depth", type=int, default=1, help="link levels to follow (default: 1)"
    )
    args = parser.parse_args(argv)
    if args.depth < 0:
        print("error: --depth must be 0 or more", file=sys.stderr)
        return 2
    try:
        start = args.root or Path.cwd()
        root = Path(git(start, "rev-parse", "--show-toplevel").strip())
        guides = list_guides(root, args.base, args.depth)
    except (GuideError, OSError) as error:
        print(f"error: {error}", file=sys.stderr)
        return 2
    if not guides:
        print("no binding guides found")
        return 0
    for guide, reasons in guides:
        print(guide.as_posix())
        for reason in reasons:
            print(f"  {reason}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
