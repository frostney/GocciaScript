"""Syntax eligibility filter for test262 tests against GocciaScript.

Scans test source for constructs that GocciaScript intentionally excludes
(var, function keyword, traditional loops, loose equality, eval, etc.)
and checks test262 feature metadata against the GocciaScript feature map.
"""

from __future__ import annotations

import re

# ---------------------------------------------------------------------------
# Feature support map
# ---------------------------------------------------------------------------

SUPPORTED_FEATURES: dict[str, bool] = {
    # Core language -- supported
    "let": True,
    "const": True,
    "arrow-function": True,
    "class": True,
    "class-fields-public": True,
    "class-fields-private": True,
    "class-static-fields-public": True,
    "class-static-fields-private": True,
    "class-methods-private": True,
    "class-static-methods-private": True,
    "computed-property-names": True,
    "default-parameters": True,
    "destructuring-binding": True,
    "destructuring-assignment": True,
    "exponentiation": True,
    "for-of": True,
    "logical-assignment-operators": True,
    "numeric-separator-literal": True,
    "object-rest": True,
    "object-spread": True,
    "optional-catch-binding": True,
    "optional-chaining": True,
    "coalesce-expression": True,
    "nullish-coalescing": True,
    "rest-parameters": True,
    "spread": True,
    "template": True,
    "String.prototype.trim": True,
    "String.prototype.includes": True,
    "String.prototype.startsWith": True,
    "String.prototype.endsWith": True,
    "String.prototype.repeat": True,
    "String.prototype.padStart": True,
    "String.prototype.padEnd": True,
    "String.prototype.trimStart": True,
    "String.prototype.trimEnd": True,
    "String.prototype.matchAll": True,
    "String.prototype.replaceAll": True,
    "String.prototype.at": True,
    "Array.from": True,
    "Array.of": True,
    "Array.prototype.includes": True,
    "Array.prototype.find": True,
    "Array.prototype.findIndex": True,
    "Array.prototype.flat": True,
    "Array.prototype.flatMap": True,
    "Array.prototype.at": True,
    "Array.prototype.findLast": True,
    "Array.prototype.findLastIndex": True,
    "Array.prototype.values": True,
    "Array.prototype.keys": True,
    "Array.prototype.entries": True,
    "change-array-by-copy": True,
    "Array.fromAsync": True,
    "Map": True,
    "Set": True,
    "Symbol": True,
    "Symbol.iterator": True,
    "Symbol.species": True,
    "Symbol.asyncIterator": True,
    "Symbol.hasInstance": True,
    "Symbol.toPrimitive": True,
    "Symbol.toStringTag": True,
    "Symbol.isConcatSpreadable": True,
    "Symbol.match": True,
    "Symbol.replace": True,
    "Symbol.search": True,
    "Symbol.split": True,
    "Symbol.for": True,
    "well-known-symbol": True,
    "Promise": True,
    "Promise.allSettled": True,
    "Promise.any": True,
    "Promise.prototype.finally": True,
    "promise-try": True,
    "JSON": True,
    "Object.assign": True,
    "Object.entries": True,
    "Object.fromEntries": True,
    "Object.hasOwn": True,
    "Object.is": True,
    "Object.keys": True,
    "Object.values": True,
    "Number.isFinite": True,
    "Number.isInteger": True,
    "Number.isNaN": True,
    "Number.isSafeInteger": True,
    "Number.parseFloat": True,
    "Number.parseInt": True,
    "Math.trunc": True,
    "Math.sign": True,
    "Math.cbrt": True,
    "Math.log2": True,
    "Math.log10": True,
    "Math.clz32": True,
    "Math.fround": True,
    "Math.imul": True,
    "Math.cosh": True,
    "Math.sinh": True,
    "Math.tanh": True,
    "Math.acosh": True,
    "Math.asinh": True,
    "Math.atanh": True,
    "Math.hypot": True,
    "Math.expm1": True,
    "Math.log1p": True,
    "RegExp": True,
    "regexp-dotall": True,
    "regexp-lookbehind": True,
    "regexp-named-groups": True,
    "regexp-unicode-property-escapes": True,
    "regexp-match-indices": True,
    "regexp-v-flag": True,
    "ArrayBuffer": True,
    "SharedArrayBuffer": True,
    "TypedArray": True,
    "TypedArray.prototype.at": True,
    "Temporal": True,
    "iterator-helpers": True,
    "decorators": True,
    "globalThis": True,
    "structuredClone": True,
    "async-functions": True,
    "async-await": True,
    "async-iteration": True,
    "top-level-await": True,
    "super": True,
    "new.target": True,
    "Uint8Array": True,
    "Int8Array": True,
    "Uint16Array": True,
    "Int16Array": True,
    "Uint32Array": True,
    "Int32Array": True,
    "Float32Array": True,
    "Float64Array": True,
    "Uint8ClampedArray": True,
    "hashbang": True,
    "String.raw": True,
    "template-literal": True,

    # Core language -- NOT supported
    "generators": False,
    "BigInt": False,
    "Proxy": False,
    "Reflect": False,
    "Reflect.construct": False,
    "Reflect.apply": False,
    "Reflect.defineProperty": False,
    "Reflect.deleteProperty": False,
    "Reflect.get": False,
    "Reflect.getOwnPropertyDescriptor": False,
    "Reflect.getPrototypeOf": False,
    "Reflect.has": False,
    "Reflect.isExtensible": False,
    "Reflect.ownKeys": False,
    "Reflect.preventExtensions": False,
    "Reflect.set": False,
    "Reflect.setPrototypeOf": False,
    "WeakMap": False,
    "WeakSet": False,
    "WeakRef": False,
    "FinalizationRegistry": False,
    "dynamic-import": False,
    "import.meta": False,
    "import-assertions": False,
    "import-attributes": False,
    "json-modules": False,
    "ShadowRealm": False,
    "tail-call-optimization": False,
    "Atomics": False,
    "DataView": False,
    "cross-realm": False,
    "caller": False,
    "IsHTMLDDA": False,
    "resizable-arraybuffer": False,
    "arraybuffer-transfer": False,
    "Float16Array": False,
    "Intl": False,
    "Intl.DateTimeFormat": False,
    "Intl.NumberFormat": False,
    "Intl.RelativeTimeFormat": False,
    "Intl.ListFormat": False,
    "Intl.Locale": False,
    "Intl.Segmenter": False,
    "Intl.DisplayNames": False,
    "Intl.DurationFormat": False,
    "Intl-enumeration": False,
    "Symbol.matchAll": False,
    "String.prototype.isWellFormed": False,
    "String.prototype.toWellFormed": False,
    "__proto__": False,
    "__getter__": False,
    "__setter__": False,
    "legacy-regexp": False,
    "regexp-duplicate-named-groups": False,
    "regexp-modifiers": False,
    "error-cause": False,
    "symbols-as-weakmap-keys": False,
    "disposition": False,
    "explicit-resource-management": False,
    "set-methods": False,
    "Map.groupBy": False,
}

# Flags that mean we should skip the test
SKIP_FLAGS: set[str] = {
    "module",          # ES module semantics differ
    "raw",             # No harness prepended -- unusual tests
    "noStrict",        # Non-strict-only tests -- GocciaScript is always strict
    "CanBlockIsFalse",
    "CanBlockIsTrue",
    "non-deterministic",
}

# ---------------------------------------------------------------------------
# Syntax patterns that indicate unsupported constructs
# ---------------------------------------------------------------------------

# These are applied to the test body (after stripping frontmatter and comments)
# to detect unsupported GocciaScript syntax.  The patterns are deliberately
# conservative: it is better to skip a valid test than to run an invalid one.

_UNSUPPORTED_PATTERNS: list[tuple[str, re.Pattern[str]]] = [
    ("var_declaration",
     re.compile(r"\bvar\s+\w")),
    ("function_declaration",
     re.compile(r"\bfunction\s+\w+\s*\(")),
    ("function_expression",
     re.compile(r"\bfunction\s*\(")),
    ("function_star",
     re.compile(r"\bfunction\s*\*")),
    ("generator_method",
     re.compile(r"\*\s*\w+\s*\(")),
    ("generator_method_bracket",
     re.compile(r"\*\s*\[")),
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
     re.compile(r"[^!]=!=[^=]")),
    ("eval_identifier",
     re.compile(r"\beval\b")),
    ("arguments_object",
     re.compile(r"\barguments\b")),
    ("with_statement",
     re.compile(r"\bwith\s*\(")),
    ("new_function",
     re.compile(r"\bnew\s+Function\s*\(")),
    ("yield_keyword",
     re.compile(r"\byield\b")),
    ("dynamic_import",
     re.compile(r"\bimport\s*\(")),
    ("labeled_statement",
     re.compile(r"^\s*\w+\s*:\s*(?:for|while|do|if|switch)\b", re.MULTILINE)),
    ("switch_statement",
     re.compile(r"\bswitch\s*\(")),
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
        if ch in ("'", '"', "`"):
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

        # Regex literal (heuristic: after = or ( or , or ; or ! or & or | or return or typeof)
        if ch == "/" and i > 0:
            prev = source[:i].rstrip()
            if prev and prev[-1] in "=({,;!&|:?[~^%*/+-":
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
        supported = SUPPORTED_FEATURES.get(feature)
        if supported is False:
            reasons.append(f"unsupported_feature:{feature}")
        # Unknown features are allowed (conservative: don't skip)
    return reasons


def check_flags(flags: list[str]) -> list[str]:
    """Return list of skip reasons for unsupported flags."""
    reasons: list[str] = []
    for flag in flags:
        if flag in SKIP_FLAGS:
            reasons.append(f"unsupported_flag:{flag}")
    return reasons


def check_syntax(source: str) -> list[str]:
    """Scan source for unsupported GocciaScript syntax patterns.

    Returns a list of skip reason strings, empty if eligible.
    """
    stripped = _strip_strings_and_comments(source)
    reasons: list[str] = []
    for name, pattern in _UNSUPPORTED_PATTERNS:
        if pattern.search(stripped):
            reasons.append(f"unsupported_syntax:{name}")
    return reasons


# Harness files we have reimplemented for GocciaScript
AVAILABLE_INCLUDES: set[str] = {
    "assert.js",
    "sta.js",
    "compareArray.js",
    "propertyHelper.js",
    "compareIterator.js",
    "isConstructor.js",
    "deepEqual.js",
    "nans.js",
}


def check_includes(includes: list[str]) -> list[str]:
    """Return list of skip reasons for harness files we haven't reimplemented."""
    reasons: list[str] = []
    for inc in includes:
        if inc not in AVAILABLE_INCLUDES:
            reasons.append(f"missing_harness:{inc}")
    return reasons


# Path segments that indicate categorically incompatible tests
SKIP_PATH_SEGMENTS: set[str] = {
    "eval-code",        # All eval-code tests use eval
    "asi",              # ASI tests -- GocciaScript requires semicolons
    "generators",       # No generator support
    "for-in",           # No for-in support
}


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
) -> tuple[bool, list[str]]:
    """Check whether a test262 test is eligible to run in GocciaScript.

    Returns ``(eligible, skip_reasons)``.
    """
    reasons: list[str] = []

    if test_id:
        reasons.extend(check_path(test_id))

    reasons.extend(check_flags(flags))
    reasons.extend(check_features(features))
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
        reasons.extend(check_syntax(source))

    return (len(reasons) == 0, reasons)
