"""Syntax eligibility filter for test262 tests against GocciaScript.

Scans test source for constructs that GocciaScript intentionally excludes
(var, function keyword, traditional loops, loose equality, eval, etc.)
and checks test262 metadata against the GocciaScript compatibility roadmap.
"""

from __future__ import annotations

import json
import re
from pathlib import Path

from test262_harness_map import AVAILABLE_INCLUDES

# ---------------------------------------------------------------------------
# Compatibility roadmap
# ---------------------------------------------------------------------------

ROADMAP_PATH = Path(__file__).with_name("test262_compatibility_roadmap.json")


def _load_compatibility_roadmap() -> dict:
    """Load the required test262 compatibility roadmap."""
    try:
        return json.loads(ROADMAP_PATH.read_text(encoding="utf-8"))
    except OSError as exc:
        raise RuntimeError(
            f"test262 compatibility roadmap is required: {ROADMAP_PATH}"
        ) from exc
    except json.JSONDecodeError as exc:
        raise RuntimeError(
            f"invalid test262 compatibility roadmap: {ROADMAP_PATH}: {exc}"
        ) from exc


COMPATIBILITY_ROADMAP: dict = _load_compatibility_roadmap()
ELIGIBLE_STATUSES: set[str] = set(COMPATIBILITY_ROADMAP["eligibleStatuses"])


def _entry_is_eligible(entry: dict | None) -> bool:
    if not entry:
        entry = COMPATIBILITY_ROADMAP["unknownFeature"]
    if "eligible" in entry:
        return bool(entry["eligible"])
    return str(entry.get("status", "")) in ELIGIBLE_STATUSES


def _roadmap_entry(kind: str, key: str) -> dict:
    section = COMPATIBILITY_ROADMAP.get(kind, {})
    entry = section.get(key)
    if entry is None:
        return COMPATIBILITY_ROADMAP["unknownFeature"]
    return entry


def _roadmap_ineligible_keys(kind: str) -> set[str]:
    return {
        name
        for name, entry in COMPATIBILITY_ROADMAP.get(kind, {}).items()
        if not _entry_is_eligible(entry)
    }


# ---------------------------------------------------------------------------
# Syntax patterns that indicate unsupported constructs
# ---------------------------------------------------------------------------

# These are applied to the test body (after stripping frontmatter and comments)
# to detect unsupported GocciaScript syntax.  The patterns are deliberately
# conservative: it is better to skip a valid test than to run an invalid one.

# Patterns that are always unsupported regardless of compatibility flags.
_ALWAYS_UNSUPPORTED_PATTERNS: list[tuple[str, re.Pattern[str]]] = [
    ("while_loop",
     re.compile(r"\bwhile\s*\(")),
    ("do_while_loop",
     re.compile(r"\bdo\s*\{")),
    ("traditional_for_loop",
     re.compile(r"\bfor\s*\([^)]*;[^)]*;[^)]*\)")),
    ("for_in_loop",
     re.compile(r"\bfor\s*\([^)]*\bin\b")),
    ("loose_equality",
     re.compile(r"[^!=<>]==[^=]")),
    ("loose_inequality",
     re.compile(r"(?<!=)!=[^=]")),
    ("eval_identifier",
     re.compile(r"\beval\b")),
    ("arguments_object",
     re.compile(r"\barguments\b")),
    ("with_statement",
     re.compile(r"\bwith\s*\(")),
    ("labeled_statement",
     re.compile(r"^\s*\w+\s*:\s*(?:for|while|do|if|switch)\b", re.MULTILINE)),
    # Labeled `break <label>` / `continue <label>` — catches the reference
    # site even when the labeled definition is on a plain block
    # (e.g. `label: { ... break label; ... }`), which the
    # `labeled_statement` pattern deliberately doesn't cover to avoid
    # false-positives on multi-line object literals.
    # Whitespace class deliberately excludes line terminators (\r, \n,
    # \u2028, \u2029): per ES2026 §13.9 / §13.10, no LineTerminator is
    # allowed between `break`/`continue` and the label, otherwise ASI
    # inserts a semicolon and the label is just the next statement.
    ("labeled_break",
     re.compile(r"\bbreak[ \t\v\f\u00A0\uFEFF]+[A-Za-z_$][\w$]*")),
    ("labeled_continue",
     re.compile(r"\bcontinue[ \t\v\f\u00A0\uFEFF]+[A-Za-z_$][\w$]*")),
]

# Patterns gated by --compat-function (function declarations / expressions).
_COMPAT_FUNCTION_PATTERNS: list[tuple[str, re.Pattern[str]]] = [
    ("function_declaration",
     re.compile(r"\bfunction\s+\w+\s*\(")),
    ("function_expression",
     re.compile(r"\bfunction\s*\(")),
]

# Patterns gated by --unsafe-function-constructor.  Both `new Function(...)`
# and the bare `Function(...)` call form construct a Function exotic via
# the same dynamic-code path (per ES2026 §20.2.1.1 the call form delegates
# to the construct form), so both must be filtered when this gate is off.
_UNSAFE_FUNCTION_CONSTRUCTOR_PATTERNS: list[tuple[str, re.Pattern[str]]] = [
    ("new_function",
     re.compile(r"\b(?:new\s+)?Function\s*\(")),
]


def _strip_strings_and_comments(source: str) -> str:
    """Crudely strip string literals and comments to avoid false positives.

    This is a heuristic -- not a full parser -- but handles the common cases
    in test262 tests.
    """
    result: list[str] = []
    i = 0
    length = len(source)

    while i < length:
        ch = source[i]

        # Single-line comment
        if ch == "/" and i + 1 < length and source[i + 1] == "/":
            while i < length and source[i] != "\n":
                i += 1
            continue

        # Multi-line comment
        if ch == "/" and i + 1 < length and source[i + 1] == "*":
            i += 2
            while i + 1 < length and not (source[i] == "*" and source[i + 1] == "/"):
                i += 1
            i += 2
            continue

        # String literals
        if ch in ("'", '"'):
            quote = ch
            i += 1
            while i < length:
                if source[i] == "\\" and i + 1 < length:
                    i += 2
                    continue
                if source[i] == quote:
                    i += 1
                    break
                i += 1
            continue

        # Template literals — strip literal chunks but preserve ${...} expressions
        if ch == "`":
            i += 1
            while i < length:
                if source[i] == "\\" and i + 1 < length:
                    i += 2
                    continue
                if source[i] == "$" and i + 1 < length and source[i + 1] == "{":
                    # Enter substitution: skip ${ but keep expression visible
                    i += 2
                    depth = 1
                    while i < length and depth > 0:
                        if source[i] == "{":
                            depth += 1
                        elif source[i] == "}":
                            depth -= 1
                            if depth == 0:
                                i += 1
                                break
                        result.append(source[i])
                        i += 1
                    continue
                if source[i] == "`":
                    i += 1
                    break
                i += 1
            continue

        # Regex literal heuristic: after operators, keywords, or at BOF
        _REGEX_PRECEDING_KEYWORDS = {"return", "typeof", "throw", "delete", "new", "in", "instanceof", "case", "void"}
        if ch == "/":
            prev = source[:i].rstrip()
            is_regex = False
            if not prev:
                is_regex = True  # BOF
            elif prev[-1] in "=({,;!&|:?[~^%*/+-":
                is_regex = True
            else:
                # Check if preceded by a keyword
                m_kw = re.search(r"\b(\w+)\s*$", prev)
                if m_kw and m_kw.group(1) in _REGEX_PRECEDING_KEYWORDS:
                    is_regex = True
            if is_regex:
                i += 1
                while i < length:
                    if source[i] == "\\" and i + 1 < length:
                        i += 2
                        continue
                    if source[i] == "/":
                        i += 1
                        # Skip flags
                        while i < length and source[i].isalpha():
                            i += 1
                        break
                    i += 1
                continue

        result.append(ch)
        i += 1

    return "".join(result)


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

def check_features(features: list[str]) -> list[str]:
    """Return list of skip reasons for unsupported features."""
    reasons: list[str] = []
    for feature in features:
        entry = _roadmap_entry("features", feature)
        if not _entry_is_eligible(entry):
            reasons.append(f"unsupported_feature:{feature}")
    return reasons


def check_flags(flags: list[str]) -> list[str]:
    """Return list of skip reasons for unsupported flags."""
    reasons: list[str] = []
    for flag in flags:
        entry = _roadmap_entry("flags", flag)
        if not _entry_is_eligible(entry):
            reasons.append(f"unsupported_flag:{flag}")
    return reasons


def classify_skip_reason(reason: str, test_id: str = "") -> dict[str, str]:
    """Return roadmap status/target metadata for a skip reason."""
    status = "unsupported"
    target = "unassigned"
    value = ""
    kind = "unknown"

    if ":" in reason:
        prefix, value = reason.split(":", 1)
    else:
        prefix = reason

    if prefix == "unsupported_feature":
        kind = "feature"
        entry = _roadmap_entry("features", value)
        status = str(entry.get("status", status))
        target = str(entry.get("target", target))
    elif prefix == "unsupported_flag":
        kind = "flag"
        entry = _roadmap_entry("flags", value)
        status = str(entry.get("status", status))
        target = str(entry.get("target", target))
    elif prefix == "unsupported_syntax":
        kind = "syntax"
        entry = _roadmap_entry("syntax", value)
        status = str(entry.get("status", "excluded-by-language-design"))
        target = str(entry.get("target", "excluded-syntax"))
    elif prefix == "skip_path":
        kind = "path"
        entry = _roadmap_entry("pathSegments", value)
        status = str(entry.get("status", status))
        target = str(entry.get("target", target))
    elif prefix == "missing_harness":
        kind = "harness"
        entry = _roadmap_entry("harnessIncludes", value)
        status = str(entry.get("status", status))
        target = str(entry.get("target", "harness"))
    elif prefix == "negative_resolution":
        kind = "negative"
        status = "unsupported"
        target = "modules"
    elif prefix.startswith("negative_"):
        kind = "negative"
        status = "unsupported"
        target = "roadmap-hygiene"

    area_parts = test_id.replace("\\", "/").split("/")[:2]
    area = "/".join(part for part in area_parts if part)
    return {
        "reason": reason,
        "kind": kind,
        "value": value,
        "status": status,
        "target": target,
        "area": area or "unknown",
    }


def check_syntax(
    source: str,
    compat_function: bool = False,
    unsafe_function_constructor: bool = False,
) -> list[str]:
    """Scan source for unsupported GocciaScript syntax patterns.

    Returns a list of skip reason strings, empty if eligible. When
    ``compat_function`` is enabled, ``function`` declarations and expressions
    are accepted. When ``unsafe_function_constructor`` is enabled,
    ``new Function(...)`` is accepted.
    """
    stripped = _strip_strings_and_comments(source)
    patterns: list[tuple[str, re.Pattern[str]]] = list(_ALWAYS_UNSUPPORTED_PATTERNS)
    if not compat_function:
        patterns.extend(_COMPAT_FUNCTION_PATTERNS)
    if not unsafe_function_constructor:
        patterns.extend(_UNSAFE_FUNCTION_CONSTRUCTOR_PATTERNS)
    reasons: list[str] = []
    for name, pattern in patterns:
        if pattern.search(stripped):
            reasons.append(f"unsupported_syntax:{name}")
    return reasons


def check_includes(includes: list[str]) -> list[str]:
    """Return list of skip reasons for harness files we haven't reimplemented."""
    reasons: list[str] = []
    for inc in includes:
        if inc not in AVAILABLE_INCLUDES:
            entry = _roadmap_entry("harnessIncludes", inc)
            if not _entry_is_eligible(entry):
                reasons.append(f"missing_harness:{inc}")
    return reasons


# Path segments that indicate categorically incompatible tests.
# NOTE: "asi" is deliberately absent -- when --asi is enabled, those tests
# are eligible.  The syntax filter already catches unsupported constructs
# that happen to appear inside ASI tests (var, function, etc.).
SKIP_PATH_SEGMENTS: set[str] = _roadmap_ineligible_keys("pathSegments")


def check_path(test_id: str) -> list[str]:
    """Return skip reasons for tests in categorically incompatible directories."""
    reasons: list[str] = []
    parts = test_id.replace("\\", "/").split("/")
    for part in parts:
        if part in SKIP_PATH_SEGMENTS:
            reasons.append(f"skip_path:{part}")
    return reasons


def is_eligible(
    source: str,
    features: list[str],
    flags: list[str],
    includes: list[str],
    negative: dict | None,
    test_id: str = "",
    compat_function: bool = False,
    unsafe_function_constructor: bool = False,
) -> tuple[bool, list[str]]:
    """Check whether a test262 test is eligible to run in GocciaScript.

    Returns ``(eligible, skip_reasons)``.
    """
    reasons: list[str] = []

    if test_id:
        reasons.extend(check_path(test_id))

    reasons.extend(check_flags(flags))
    reasons.extend(check_features(features))
    if not reasons:
        reasons.extend(check_includes(includes))

    # Negative parse/resolution tests need special handling
    if negative is not None:
        phase = negative.get("phase", "")
        if phase == "resolution":
            reasons.append("negative_resolution")
        # parse and runtime are handled by the runner

    # Syntax check (only if not already skipped for other reasons,
    # to save time on large suites)
    if not reasons:
        reasons.extend(
            check_syntax(
                source,
                compat_function=compat_function,
                unsafe_function_constructor=unsafe_function_constructor,
            )
        )

    return (len(reasons) == 0, reasons)
