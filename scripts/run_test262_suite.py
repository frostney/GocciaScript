#!/usr/bin/env python3
"""Run GocciaScript against the official TC39 test262 conformance suite.

Clones (or reuses) the test262 repository, parses YAML frontmatter from
each test, filters out tests requiring syntax or features that GocciaScript
intentionally excludes, wraps eligible tests with the GocciaScript-compatible
harness using Goccia TestAssertions (describe/test/expect), generates temp
files, and executes them in batch via ``TestRunner``.

Negative parse-phase tests are run individually via ``ScriptLoader`` since
they expect parsing to fail before any test harness code can execute.

Results are reported as JSON (console + optional file).

Usage:
    python3 scripts/run_test262_suite.py
    python3 scripts/run_test262_suite.py --suite-dir /path/to/test262
    python3 scripts/run_test262_suite.py --filter "built-ins/Array/*"
    python3 scripts/run_test262_suite.py --output results.json --verbose
"""

from __future__ import annotations

import argparse
import fnmatch
import json
import os
import re
import subprocess
import sys
import tempfile
import time
from collections import Counter
from pathlib import Path

# Sibling modules
sys.path.insert(0, str(Path(__file__).resolve().parent))
from test262_frontmatter import parse_frontmatter, strip_frontmatter  # noqa: E402
from test262_syntax_filter import is_eligible  # noqa: E402

SUITE_REPO_URL = "https://github.com/tc39/test262.git"
SUITE_BRANCH = "main"
DEFAULT_TIMEOUT_SECONDS = 10
DEFAULT_CATEGORIES = ("language", "built-ins")

# Paths relative to this script
SCRIPT_DIR = Path(__file__).resolve().parent
REPO_ROOT = SCRIPT_DIR.parent
HARNESS_DIR = SCRIPT_DIR / "test262_harness"


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def run(
    command: list[str], cwd: Path | None = None
) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        command,
        cwd=str(cwd) if cwd else None,
        text=True,
        capture_output=True,
        check=True,
    )


def ensure_suite_checkout(
    suite_dir: Path | None,
) -> tuple[Path, tempfile.TemporaryDirectory[str] | None]:
    if suite_dir is not None:
        return suite_dir.resolve(), None

    temp_dir = tempfile.TemporaryDirectory(prefix="test262-suite.")
    checkout_dir = Path(temp_dir.name) / "repo"
    print(f"Cloning test262 (shallow) into {checkout_dir} ...")
    run(
        [
            "git",
            "clone",
            "--depth",
            "1",
            "--branch",
            SUITE_BRANCH,
            SUITE_REPO_URL,
            str(checkout_dir),
        ]
    )
    print("Clone complete.")
    return checkout_dir, temp_dir


def ensure_binary(name: str, binary_path: Path | None, build_target: str) -> Path:
    """Locate or build a GocciaScript binary (TestRunner or ScriptLoader)."""
    if binary_path is not None:
        resolved = binary_path.resolve()
        if not resolved.is_file():
            raise FileNotFoundError(f"{name} not found: {resolved}")
        return resolved

    default_path = REPO_ROOT / "build" / name
    if default_path.is_file():
        return default_path

    print(f"Building {name} ...")
    run([str(REPO_ROOT / "build.pas"), build_target], cwd=REPO_ROOT)
    if not default_path.is_file():
        raise FileNotFoundError(
            f"Build succeeded but {name} not found at {default_path}"
        )
    return default_path


# ---------------------------------------------------------------------------
# Harness loading
# ---------------------------------------------------------------------------


def load_harness_files() -> dict[str, str]:
    """Load all GocciaScript-compatible harness files from disk."""
    harness: dict[str, str] = {}
    for path in sorted(HARNESS_DIR.glob("*.js")):
        harness[path.name] = path.read_text(encoding="utf-8")
    return harness


# Map test262 include names to our harness file names
INCLUDE_MAP: dict[str, str] = {
    "assert.js": "assert.js",
    "sta.js": "assert.js",  # $ERROR and $DONOTEVALUATE are in our assert.js
    "compareArray.js": "assert.js",  # assert.compareArray is in our assert.js
    "propertyHelper.js": "propertyHelper.js",
    "compareIterator.js": "compareIterator.js",
    "isConstructor.js": "isConstructor.js",
    "deepEqual.js": "deepEqual.js",
    "nans.js": "nans.js",
    "testTypedArray.js": "testTypedArray.js",
    "temporalHelpers.js": "temporalHelpers.js",
    "promiseHelper.js": "promiseHelper.js",
    "dateConstants.js": "dateConstants.js",
    "decimalToHexString.js": "decimalToHexString.js",
    "fnGlobalObject.js": "fnGlobalObject.js",
    "nativeFunctionMatcher.js": "nativeFunctionMatcher.js",
    "byteConversionValues.js": "byteConversionValues.js",
}


def build_harness_source(includes: list[str], harness_files: dict[str, str]) -> str:
    """Build the combined harness source for a test, given its ``includes:``."""
    parts: list[str] = [harness_files["assert.js"]]
    seen: set[str] = {"assert.js"}

    for inc in includes:
        mapped = INCLUDE_MAP.get(inc)
        if mapped and mapped not in seen and mapped in harness_files:
            parts.append(harness_files[mapped])
            seen.add(mapped)

    return "\n".join(parts)


# ---------------------------------------------------------------------------
# Test wrapping (uses Goccia TestAssertions: describe/test/expect)
# ---------------------------------------------------------------------------


def _escape_js_string(s: str) -> str:
    return (
        s.replace("\\", "\\\\")
        .replace('"', '\\"')
        .replace("\n", "\\n")
        .replace("\r", "\\r")
    )


def _strip_use_strict(body: str) -> str:
    body = re.sub(r'^(\s*)"use strict"\s*;?\s*\n', "", body)
    return re.sub(r"^(\s*)'use strict'\s*;?\s*\n", "", body)


def wrap_positive_test(
    harness_source: str, test_body: str, test_id: str, description: str
) -> str:
    """Wrap a positive test in describe/test with Goccia TestAssertions."""
    body = _strip_use_strict(test_body)
    desc = _escape_js_string(description or test_id)
    tid = _escape_js_string(test_id)

    return f"""{harness_source}

describe("test262: {tid}", () => {{
  test("{desc}", () => {{
{body}
  }});
}});
"""


def wrap_negative_runtime_test(
    harness_source: str,
    test_body: str,
    test_id: str,
    description: str,
    error_type: str,
) -> str:
    """Wrap a negative runtime test: body inside expect(() => ...).toThrow()."""
    body = _strip_use_strict(test_body)
    desc = _escape_js_string(f"{description} [negative: runtime {error_type}]")
    tid = _escape_js_string(test_id)

    return f"""{harness_source}

describe("test262: {tid}", () => {{
  test("{desc}", () => {{
    expect(() => {{
{body}
    }}).toThrow({error_type});
  }});
}});
"""


# ---------------------------------------------------------------------------
# Test discovery
# ---------------------------------------------------------------------------


def discover_tests(
    suite_dir: Path, categories: tuple[str, ...], filter_glob: str | None
) -> list[Path]:
    test_dir = suite_dir / "test"
    if not test_dir.is_dir():
        raise FileNotFoundError(f"test262 test/ directory not found: {test_dir}")

    tests: list[Path] = []
    for category in categories:
        category_dir = test_dir / category
        if not category_dir.is_dir():
            print(
                f"Warning: category directory not found: {category_dir}",
                file=sys.stderr,
            )
            continue
        for path in sorted(category_dir.rglob("*.js")):
            if path.name.startswith("_"):
                continue
            if filter_glob and not _matches_glob(path, test_dir, filter_glob):
                continue
            tests.append(path)

    return tests


def _matches_glob(path: Path, base: Path, pattern: str) -> bool:
    rel = str(path.relative_to(base))
    return fnmatch.fnmatch(rel, pattern) or fnmatch.fnmatch(
        rel, f"*/{pattern}"
    )


# ---------------------------------------------------------------------------
# Execution
# ---------------------------------------------------------------------------


def run_negative_parse_test(
    script_loader: Path,
    source: str,
    expected_error: str,
    timeout: int,
    asi: bool = False,
) -> tuple[bool, str, float]:
    """Run a negative parse-phase test via ScriptLoader stdin."""
    start = time.monotonic()
    cmd = [str(script_loader)]
    if asi:
        cmd.append("--asi")
    try:
        process = subprocess.run(
            cmd,
            input=source.encode("utf-8", errors="replace"),
            capture_output=True,
            timeout=timeout,
        )
        duration = time.monotonic() - start
        if process.returncode == 0:
            return (
                False,
                "Expected parse error but script ran successfully",
                duration,
            )
        output = (
            process.stdout.decode("utf-8", errors="replace")
            + process.stderr.decode("utf-8", errors="replace")
        ).strip()
        return True, output, duration
    except subprocess.TimeoutExpired:
        return False, "TIMEOUT", time.monotonic() - start


# ---------------------------------------------------------------------------
# Main evaluation loop
# ---------------------------------------------------------------------------


def evaluate_suite(
    test_runner: Path,
    script_loader: Path,
    suite_dir: Path,
    categories: tuple[str, ...],
    filter_glob: str | None,
    timeout: int,
    max_tests: int,
    verbose: bool,
    harness_files: dict[str, str],
    asi: bool = False,
) -> dict:
    test_dir = suite_dir / "test"
    all_tests = discover_tests(suite_dir, categories, filter_glob)

    summary: dict = {
        "total_discovered": len(all_tests),
        "total_eligible": 0,
        "total_skipped": 0,
        "positive_tests": 0,
        "negative_runtime_tests": 0,
        "negative_parse_tests": 0,
        "total_run": 0,
        "passed": 0,
        "failed": 0,
        "errors": 0,
        "timeouts": 0,
    }
    skip_reasons_counter: Counter[str] = Counter()

    positive_tests: list[tuple[Path, dict, str]] = []
    negative_runtime_tests: list[tuple[Path, dict, str]] = []
    negative_parse_tests: list[tuple[Path, dict, str]] = []

    print(f"Discovered {len(all_tests)} test files.  Filtering ...")

    # Phase 1: Filter and categorize
    for test_path in all_tests:
        try:
            source = test_path.read_text(encoding="utf-8")
        except (OSError, UnicodeDecodeError):
            skip_reasons_counter["read_error"] += 1
            summary["total_skipped"] += 1
            continue

        metadata = parse_frontmatter(source)
        body = strip_frontmatter(source)
        test_id = str(test_path.relative_to(test_dir))

        eligible, reasons = is_eligible(
            body,
            metadata.get("features", []),
            metadata.get("flags", []),
            metadata.get("includes", []),
            metadata.get("negative"),
            test_id=test_id,
        )

        if not eligible:
            summary["total_skipped"] += 1
            for reason in reasons:
                skip_reasons_counter[reason] += 1
            continue
        negative = metadata.get("negative")

        if negative is not None and negative.get("phase") == "parse":
            negative_parse_tests.append((test_path, metadata, test_id))
        elif negative is not None and negative.get("phase") == "runtime":
            negative_runtime_tests.append((test_path, metadata, test_id))
        else:
            positive_tests.append((test_path, metadata, test_id))

        total_eligible = (
            len(positive_tests)
            + len(negative_runtime_tests)
            + len(negative_parse_tests)
        )
        if max_tests > 0 and total_eligible >= max_tests:
            break

    total_eligible = (
        len(positive_tests)
        + len(negative_runtime_tests)
        + len(negative_parse_tests)
    )
    summary["total_eligible"] = total_eligible
    summary["positive_tests"] = len(positive_tests)
    summary["negative_runtime_tests"] = len(negative_runtime_tests)
    summary["negative_parse_tests"] = len(negative_parse_tests)

    pct = 100 * total_eligible / max(len(all_tests), 1)
    print(f"Eligible: {total_eligible} / {len(all_tests)} ({pct:.1f}%)")
    print(
        f"  Positive: {len(positive_tests)}, "
        f"Negative-runtime: {len(negative_runtime_tests)}, "
        f"Negative-parse: {len(negative_parse_tests)}"
    )
    print()

    results: list[dict] = []
    start_time = time.monotonic()

    # Phase 2: Positive + negative-runtime via TestRunner (batch)
    batch_tests = positive_tests + negative_runtime_tests
    if batch_tests:
        print(
            f"Generating {len(batch_tests)} wrapped test files for TestRunner ..."
        )

        with tempfile.TemporaryDirectory(prefix="test262-goccia.") as tmp:
            temp_dir = Path(tmp)
            file_to_test_id: dict[str, str] = {}

            for test_path, metadata, test_id in batch_tests:
                source = test_path.read_text(encoding="utf-8")
                body = strip_frontmatter(source)
                description = metadata.get("description", test_id)
                if isinstance(description, dict):
                    description = str(description)
                negative = metadata.get("negative")
                includes = metadata.get("includes", [])

                harness_source = build_harness_source(includes, harness_files)

                if negative is not None and negative.get("phase") == "runtime":
                    error_type = negative.get("type", "TypeError")
                    wrapped = wrap_negative_runtime_test(
                        harness_source, body, test_id, description, error_type
                    )
                else:
                    wrapped = wrap_positive_test(
                        harness_source, body, test_id, description
                    )

                safe_name = test_id.replace("/", "__").replace("\\", "__")
                out_path = temp_dir / safe_name
                out_path.write_text(wrapped, encoding="utf-8")
                file_to_test_id[safe_name] = test_id

            print(f"Running TestRunner on {len(batch_tests)} tests ...")
            json_output = temp_dir / "__results.json"

            runner_cmd = [
                str(test_runner),
                str(temp_dir),
                "--no-progress",
                "--silent",
                f"--output={json_output}",
            ]
            if asi:
                runner_cmd.append("--asi")

            try:
                runner_timeout = max(timeout * len(batch_tests), 120) + 60
                process = subprocess.run(
                    runner_cmd,
                    capture_output=True,
                    timeout=runner_timeout,
                )
                runner_output = (
                    process.stdout.decode("utf-8", errors="replace")
                    + process.stderr.decode("utf-8", errors="replace")
                ).strip()
            except subprocess.TimeoutExpired:
                runner_output = "TestRunner global timeout"
                process = None

            if json_output.is_file():
                raw_json = json_output.read_bytes().decode("utf-8", errors="replace")
                # Strip control characters that test262 tests may inject
                clean_json = re.sub(r"[\x00-\x08\x0b\x0c\x0e-\x1f]", "", raw_json)
                tr_results = json.loads(clean_json)
                tr_passed = int(tr_results.get("passed", 0))
                tr_failed = int(tr_results.get("failed", 0))
                tr_skipped = int(tr_results.get("skipped", 0))
                tr_failed_tests = tr_results.get("failedTests", [])

                summary["passed"] += tr_passed
                summary["failed"] += tr_failed

                # Build failed-test ID set from TestRunner output
                failed_set: set[str] = set()
                failed_messages: dict[str, str] = {}
                for name in tr_failed_tests:
                    # Case 1: "test262: <test_id> > <description>"
                    m = re.match(r"test262:\s*(.+?)\s*>", name)
                    if m:
                        failed_set.add(m.group(1))
                        failed_messages[m.group(1)] = name
                        continue
                    # Case 2: "<filepath>: <error>" (parse/load error)
                    # Extract filename, convert __ back to /
                    m2 = re.search(r"[/\\]([^/\\]+\.js)(?:\s*:\s*(.+))?$", name)
                    if m2:
                        fname = m2.group(1)
                        test_id_guess = fname.replace("__", "/")
                        failed_set.add(test_id_guess)
                        msg = m2.group(2) or name
                        failed_messages[test_id_guess] = msg
                    else:
                        failed_set.add(name)
                        failed_messages[name] = name

                for _, _, test_id in batch_tests:
                    safe = test_id.replace("/", "__").replace("\\", "__")
                    if test_id in failed_set or safe in failed_set:
                        status = "FAIL"
                        msg = failed_messages.get(test_id, failed_messages.get(safe, ""))
                    else:
                        status = "PASS"
                        msg = ""
                    results.append({"id": test_id, "status": status, "message": msg})

                print(
                    f"  TestRunner: {tr_passed} passed, "
                    f"{tr_failed} failed, {tr_skipped} skipped"
                )
                if verbose:
                    for tid in sorted(failed_set)[:50]:
                        print(f"    F {tid}")
            else:
                print("Warning: TestRunner produced no JSON output")
                if runner_output:
                    for line in runner_output.split("\n")[:10]:
                        print(f"  {line}")
                summary["errors"] += len(batch_tests)
                for _, _, test_id in batch_tests:
                    results.append(
                        {
                            "id": test_id,
                            "status": "ERROR",
                            "message": runner_output[:300],
                        }
                    )

    # Phase 3: Negative parse-phase tests via ScriptLoader
    if negative_parse_tests:
        print(
            f"Running {len(negative_parse_tests)} negative parse tests "
            f"via ScriptLoader ..."
        )
        for test_path, metadata, test_id in negative_parse_tests:
            source = test_path.read_text(encoding="utf-8")
            body = strip_frontmatter(source)
            negative = metadata.get("negative", {})
            expected_error = negative.get("type", "SyntaxError")

            passed, output, duration = run_negative_parse_test(
                script_loader, body, expected_error, timeout, asi=asi
            )

            if output == "TIMEOUT":
                summary["timeouts"] += 1
                status = "TIMEOUT"
            elif passed:
                summary["passed"] += 1
                status = "PASS"
            else:
                summary["failed"] += 1
                status = "FAIL"

            results.append(
                {
                    "id": test_id,
                    "status": status,
                    "duration_ms": round(duration * 1000, 1),
                    "message": output[:300] if output else "",
                }
            )

            if verbose:
                marker = {"PASS": ".", "FAIL": "F", "TIMEOUT": "T"}[status]
                print(f"  {marker} {test_id}")

    total_duration = time.monotonic() - start_time
    summary["duration_seconds"] = round(total_duration, 2)
    summary["total_run"] = (
        summary["passed"]
        + summary["failed"]
        + summary["errors"]
        + summary["timeouts"]
    )
    summary["skip_reasons"] = dict(skip_reasons_counter.most_common())

    return {"summary": summary, "results": results}


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Run GocciaScript against the official TC39 test262 suite."
    )
    parser.add_argument(
        "--suite-dir",
        type=Path,
        help="Existing test262 checkout.  Clones to a temp dir if omitted.",
    )
    parser.add_argument(
        "--test-runner",
        type=Path,
        help="Prebuilt TestRunner binary.",
    )
    parser.add_argument(
        "--script-loader",
        type=Path,
        help="Prebuilt ScriptLoader binary.",
    )
    parser.add_argument(
        "--output",
        type=Path,
        help="JSON output path for the full report.",
    )
    parser.add_argument(
        "--timeout",
        type=int,
        default=int(
            os.environ.get("TEST262_TIMEOUT_SECONDS", DEFAULT_TIMEOUT_SECONDS)
        ),
        help=f"Per-test timeout in seconds (default: {DEFAULT_TIMEOUT_SECONDS}).",
    )
    parser.add_argument(
        "--filter",
        type=str,
        default=os.environ.get("TEST262_FILTER"),
        help='Glob pattern for test paths (e.g. "built-ins/Array/*").',
    )
    parser.add_argument(
        "--categories",
        type=str,
        default=",".join(DEFAULT_CATEGORIES),
        help="Comma-separated categories (default: language,built-ins).",
    )
    parser.add_argument(
        "--max-tests",
        type=int,
        default=int(os.environ.get("TEST262_MAX_TESTS", "0")),
        help="Max eligible tests to run (0 = unlimited).",
    )
    parser.add_argument(
        "--verbose",
        action="store_true",
        default=False,
        help="Print per-test results.",
    )
    parser.add_argument(
        "--asi",
        action="store_true",
        default=False,
        help="Enable automatic semicolon insertion (pass --asi to TestRunner/ScriptLoader).",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()

    suite_dir, temp_checkout = ensure_suite_checkout(args.suite_dir)
    test_runner = ensure_binary("TestRunner", args.test_runner, "testrunner")
    script_loader = ensure_binary("ScriptLoader", args.script_loader, "loader")
    harness_files = load_harness_files()
    categories = tuple(
        c.strip() for c in args.categories.split(",") if c.strip()
    )

    print(f"TestRunner:    {test_runner}")
    print(f"ScriptLoader:  {script_loader}")
    print(f"Suite:         {suite_dir}")
    print(f"Categories:    {', '.join(categories)}")
    print(f"Timeout:       {args.timeout}s per test")
    if args.filter:
        print(f"Filter:        {args.filter}")
    if args.max_tests:
        print(f"Max tests:     {args.max_tests}")
    if args.asi:
        print("ASI:           enabled")
    print()

    report = evaluate_suite(
        test_runner=test_runner,
        script_loader=script_loader,
        suite_dir=suite_dir,
        categories=categories,
        filter_glob=args.filter,
        timeout=args.timeout,
        max_tests=args.max_tests,
        verbose=args.verbose,
        harness_files=harness_files,
        asi=args.asi,
    )

    if args.output is not None:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        args.output.write_text(
            json.dumps(report, indent=2) + "\n", encoding="utf-8"
        )
        print(f"\nFull report written to: {args.output.resolve()}")

    # Print summary
    s = report["summary"]
    print()
    print("=" * 60)
    print("test262 Conformance Summary")
    print("=" * 60)
    print(f"  Discovered:       {s['total_discovered']:>6}")
    print(
        f"  Eligible:         {s['total_eligible']:>6}  "
        f"({100 * s['total_eligible'] / max(s['total_discovered'], 1):.1f}%)"
    )
    print(f"    Positive:       {s['positive_tests']:>6}")
    print(f"    Neg-runtime:    {s['negative_runtime_tests']:>6}")
    print(f"    Neg-parse:      {s['negative_parse_tests']:>6}")
    print(f"  Skipped:          {s['total_skipped']:>6}")
    print(f"  Run:              {s['total_run']:>6}")
    print(
        f"  Passed:           {s['passed']:>6}  "
        f"({100 * s['passed'] / max(s['total_run'], 1):.1f}%)"
    )
    print(f"  Failed:           {s['failed']:>6}")
    if s["errors"]:
        print(f"  Errors:           {s['errors']:>6}")
    if s["timeouts"]:
        print(f"  Timeouts:         {s['timeouts']:>6}")
    print(f"  Duration:         {s.get('duration_seconds', 0):.1f}s")
    print()

    if s.get("skip_reasons"):
        print("Top skip reasons:")
        for reason, count in sorted(
            s["skip_reasons"].items(), key=lambda x: -x[1]
        )[:15]:
            print(f"  {count:>6}  {reason}")
        print()

    failing = [
        r
        for r in report["results"]
        if r["status"] in ("FAIL", "TIMEOUT", "ERROR")
    ]
    if failing:
        print(f"Failing tests ({len(failing)} total, showing first 30):")
        for r in failing[:30]:
            print(f"  [{r['status']}] {r['id']}")
            if r.get("message"):
                for line in r["message"].split("\n")[:2]:
                    print(f"         {line[:120]}")
        print()

    if temp_checkout is not None:
        temp_checkout.cleanup()

    return 1 if s["failed"] > 0 or s["timeouts"] > 0 else 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except subprocess.CalledProcessError as error:
        if error.stdout:
            sys.stdout.write(error.stdout)
        if error.stderr:
            sys.stderr.write(error.stderr)
        raise
