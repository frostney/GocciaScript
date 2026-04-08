"""Minimal YAML frontmatter parser for test262 test files.

No external dependencies -- handles the specific YAML subset used in
test262 frontmatter (``/*--- ... ---*/`` blocks) without PyYAML.
"""

from __future__ import annotations

import re
from typing import Any

_FRONTMATTER_RE = re.compile(r"/\*---\s*\n(.*?)\n---\*/", re.DOTALL)
_INLINE_LIST_RE = re.compile(r"^\[(.*)]\s*$")


def _parse_inline_list(value: str) -> list[str]:
    """Parse ``[a, b, c]`` into ``['a', 'b', 'c']``."""
    items: list[str] = []
    for item in value.split(","):
        item = item.strip().strip("'\"")
        if item:
            items.append(item)
    return items


def _unquote(value: str) -> str:
    if len(value) >= 2 and value[0] == value[-1] and value[0] in ("'", '"'):
        return value[1:-1]
    return value


def parse_frontmatter(source: str) -> dict[str, Any]:
    """Extract and parse the YAML frontmatter from a test262 source file.

    Returns a dict with keys:
      description (str), info (str), features (list[str]),
      flags (list[str]), negative (dict | None), includes (list[str]),
      esid (str), es5id (str), es6id (str), locale (list[str]).
    Missing keys default to empty string / empty list / None.
    """
    match = _FRONTMATTER_RE.search(source)
    if not match:
        return _defaults()

    yaml_text = match.group(1)
    return _parse_yaml_block(yaml_text)


def strip_frontmatter(source: str) -> str:
    """Return *source* with the ``/*--- ... ---*/`` block removed."""
    match = _FRONTMATTER_RE.search(source)
    if not match:
        return source
    before = source[: match.start()].rstrip()
    after = source[match.end() :].lstrip("\n")
    return (before + "\n" + after).lstrip("\n")


def _defaults() -> dict[str, Any]:
    return {
        "description": "",
        "info": "",
        "features": [],
        "flags": [],
        "negative": None,
        "includes": [],
        "esid": "",
        "es5id": "",
        "es6id": "",
        "locale": [],
    }


def _parse_yaml_block(text: str) -> dict[str, Any]:
    result = _defaults()
    lines = text.split("\n")
    i = 0

    while i < len(lines):
        line = lines[i]

        # Skip blank lines and comments
        if not line.strip() or line.strip().startswith("#"):
            i += 1
            continue

        # Top-level key: value
        colon_pos = line.find(":")
        if colon_pos < 0 or line[0] == " ":
            i += 1
            continue

        key = line[:colon_pos].strip()
        rest = line[colon_pos + 1 :].strip()

        # Block scalar (| or >)
        if rest in ("|", ">", "|+", ">+", "|-", ">-"):
            i += 1
            block_lines: list[str] = []
            while i < len(lines):
                if lines[i] and not lines[i][0].isspace():
                    break
                block_lines.append(lines[i])
                i += 1
            # Dedent
            indent = 0
            for bl in block_lines:
                if bl.strip():
                    indent = len(bl) - len(bl.lstrip())
                    break
            block_text = "\n".join(
                bl[indent:] if len(bl) > indent else bl for bl in block_lines
            )
            result[key] = block_text.strip()
            continue

        # Inline list [a, b, c]
        list_match = _INLINE_LIST_RE.match(rest)
        if list_match:
            result[key] = _parse_inline_list(list_match.group(1))
            i += 1
            continue

        # Nested mapping (like negative:)
        if rest == "":
            i += 1
            nested: dict[str, str] = {}
            while i < len(lines):
                nline = lines[i]
                if not nline or (nline[0] != " " and nline[0] != "\t"):
                    break
                nline = nline.strip()
                if not nline or nline.startswith("#"):
                    i += 1
                    continue
                ncolon = nline.find(":")
                if ncolon < 0:
                    i += 1
                    continue
                nkey = nline[:ncolon].strip()
                nval = nline[ncolon + 1 :].strip()
                nested[nkey] = _unquote(nval)
                i += 1
            result[key] = nested if nested else None
            continue

        # Simple scalar
        result[key] = _unquote(rest)
        i += 1

    # Normalize list fields
    for list_key in ("features", "flags", "includes", "locale"):
        val = result.get(list_key)
        if isinstance(val, str):
            m = _INLINE_LIST_RE.match(val)
            if m:
                result[list_key] = _parse_inline_list(m.group(1))
            elif val:
                result[list_key] = [val]
            else:
                result[list_key] = []
        elif val is None:
            result[list_key] = []

    return result
