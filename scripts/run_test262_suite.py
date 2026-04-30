#!/usr/bin/env python3
"""Run GocciaScript against the official TC39 test262 conformance suite.

Clones (or reuses) the test262 repository, parses YAML frontmatter from
each test, filters out tests requiring syntax or features that GocciaScript
intentionally excludes, wraps eligible tests with the GocciaScript-compatible
harness using Goccia TestAssertions (describe/test/expect), generates temp
files, and executes them in batch via ``GocciaTestRunner``.

Negative parse-phase tests are run individually via ``GocciaScriptLoader`` since
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
import hashlib
import json
import os
import re
import subprocess
import sys
import tempfile
import threading
import time
from collections import Counter
from pathlib import Path

# Sibling modules
sys.path.insert(0, str(Path(__file__).resolve().parent))
from test262_frontmatter import parse_frontmatter, strip_frontmatter  # noqa: E402
from test262_syntax_filter import is_eligible  # noqa: E402

SUITE_REPO_URL = "https://github.com/tc39/test262.git"
SUITE_BRANCH = "main"
DEFAULT_TIMEOUT_SECONDS = 1
DEFAULT_CATEGORIES = ("language", "built-ins")

# Paths relative to this script
SCRIPT_DIR = Path(__file__).resolve().parent
REPO_ROOT = SCRIPT_DIR.parent
HARNESS_DIR = SCRIPT_DIR / "test262_harness"


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _recover_per_file_records(raw_json: str) -> list[dict] | None:
    """Salvage per-file records from a runner JSON whose top-level
    structure is broken.

    Each per-file record is emitted by the runner on its own line in
    the form ``{"file": "...", ..., "failedTests": [...]}`` (possibly
    followed by a trailing comma).  When the surrounding "failedTests"
    array gets corrupted (a known runner bug under heavy stall load),
    we cannot ``json.loads`` the whole document but the per-file
    objects are still well-formed and parseable individually.

    Returns the recovered list, or None if nothing usable was found.
    """
    records: list[dict] = []
    # Each line is either an entry like
    #     {"file": "...", "passed": 0, ...},
    # or
    #     {"file": "...", "passed": 0, ...}
    # Match a JSON object on a single line that starts with {"file":.
    line_re = re.compile(r'^\s*(\{"file":\s*".*?\})(,?)\s*$')
    for line in raw_json.splitlines():
        m = line_re.match(line)
        if not m:
            continue
        try:
            obj = json.loads(m.group(1))
        except json.JSONDecodeError:
            continue
        if isinstance(obj, dict) and "file" in obj:
            records.append(obj)
    return records or None


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
    """Locate or build a GocciaScript binary (GocciaTestRunner or GocciaScriptLoader)."""
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
    "testBigIntTypedArray.js": "testTypedArray.js",
    "temporalHelpers.js": "temporalHelpers.js",
    "promiseHelper.js": "promiseHelper.js",
    "dateConstants.js": "dateConstants.js",
    "decimalToHexString.js": "decimalToHexString.js",
    "fnGlobalObject.js": "fnGlobalObject.js",
    "nativeFunctionMatcher.js": "nativeFunctionMatcher.js",
    "byteConversionValues.js": "byteConversionValues.js",
    "proxyTrapsHelper.js": "proxyTrapsHelper.js",
    "regExpUtils.js": "regExpUtils.js",
    "detachArrayBuffer.js": "detachArrayBuffer.js",
    "doneprintHandle.js": "doneprintHandle.js",
}


def build_harness_source(includes: list[str], harness_files: dict[str, str]) -> str:
    """Build the combined harness source for a test, given its ``includes:``.

    Cross-file prototype isolation is provided by per-file engine recreation
    in the test runner combined with the per-engine ``TGocciaRealm`` —
    freeing the engine tears down the realm and unpins the per-realm
    intrinsic prototypes, so mutations cannot leak into the next file.
    """
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
    harness_source: str,
    test_body: str,
    test_id: str,
    description: str,
    is_async: bool = False,
) -> str:
    """Wrap a positive test in describe/test with Goccia TestAssertions."""
    body = _strip_use_strict(test_body)
    desc = _escape_js_string(description or test_id)
    tid = _escape_js_string(test_id)

    if is_async:
        return f"""{harness_source}

describe("test262: {tid}", () => {{
  test("{desc}", async () => {{
{body}
    await __donePromise;
  }});
}});
"""

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
    mode: str = "interpreted",
    compat_var: bool = False,
    compat_function: bool = False,
    unsafe_function_constructor: bool = False,
) -> tuple[bool, str, float]:
    """Run a negative parse-phase test via GocciaScriptLoader stdin."""
    start = time.monotonic()
    cmd = [str(script_loader)]
    if asi:
        cmd.append("--asi")
    if mode != "interpreted":
        cmd.append(f"--mode={mode}")
    if compat_var:
        cmd.append("--compat-var")
    if compat_function:
        cmd.append("--compat-function")
    if unsafe_function_constructor:
        cmd.append("--unsafe-function-constructor")
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
    mode: str = "interpreted",
    compat_var: bool = False,
    compat_function: bool = False,
    unsafe_function_constructor: bool = False,
    jobs: int | None = None,
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
            compat_function=compat_function,
            unsafe_function_constructor=unsafe_function_constructor,
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

    # Phase 2: Positive + negative-runtime tests via GocciaTestRunner.
    # Single batch invocation. Three layers of bounding, finest first:
    #
    #   1. --test-timeout  (cooperative, in-engine) — fires per test.
    #      A slow test is marked TIMEOUT and the worker keeps draining
    #      the queue. This is the *first call* for runaway JS.
    #   2. --describe-timeout (cooperative) — bounds the wrapping
    #      describe(); for test262's "1 describe + 1 test" wrappers
    #      this rarely fires but stays armed defensively.
    #   3. --timeout (cooperative, per-file) — final cooperative bound;
    #      aborts the file if all three layers fail to fire on the test
    #      (only possible if the VM is stuck not polling).
    #
    # The runner's per-worker-idle watchdog is the last-resort safety
    # net for stalls in native Pascal code that no cooperative deadline
    # can reach.  Per-file ground-truth records emitted by the runner
    # are consumed below so we never have to guess pass/fail from
    # aggregate counts.
    batch_tests = positive_tests + negative_runtime_tests
    if batch_tests:
        print(
            f"Running {len(batch_tests)} tests via GocciaTestRunner ..."
        )

        # Map test_id -> (test_path, metadata) so retries can re-render the
        # file without rebuilding the eligibility scan.
        test_ids_in_order = [tid for _, _, tid in batch_tests]
        test_meta_by_id = {tid: (tp, md) for tp, md, tid in batch_tests}

        # Final classification per test_id; tests not yet in this map still
        # need to be (re-)run.
        final_by_id: dict[str, dict] = {}

        # Process the eligible batch in fixed-size chunks instead of one
        # giant subprocess invocation.  When a single test crashes the
        # runner (OOM, native stall the cooperative timeout can't reach,
        # exit=1 on a Pascal-side error) the runner produces no JSON and
        # *every* still-pending test in that subprocess gets marked ERROR.
        # Without chunking, one bad test in test262's 35K wrappers
        # invalidates tens of thousands of others.  Chunking bounds the
        # blast radius to TEST262_BATCH_SIZE tests per crash and lets the
        # rest of the suite still report real conformance numbers.
        # Each chunk gets a fresh runner subprocess with its own
        # known_stalled set and retry budget.
        batch_size_default = 1000
        batch_size = int(os.environ.get(
            "TEST262_BATCH_SIZE", str(batch_size_default)))
        if batch_size <= 0:
            batch_size = len(test_ids_in_order) or 1
        total_chunks = (len(test_ids_in_order) + batch_size - 1) // batch_size

        for chunk_index in range(total_chunks):
            chunk_start = chunk_index * batch_size
            chunk_ids = test_ids_in_order[chunk_start:chunk_start + batch_size]
            if total_chunks > 1:
                print(
                    f"  Chunk {chunk_index + 1}/{total_chunks}: "
                    f"{len(chunk_ids)} tests "
                    f"({chunk_start + 1}..{chunk_start + len(chunk_ids)})"
                )

            # Tests we've already seen stall in native code — we exclude them
            # from retries so a single bad test cannot orphan the same worker
            # again on the next attempt.  Reset per chunk: a stall from a
            # prior chunk cannot affect tests in this one.
            known_stalled: set[str] = set()

            pending_ids = list(chunk_ids)

            # Per-worker stalls bounded by jobs count; each retry reduces the
            # queue by one stall worth of files.  Scale the ceiling with the
            # initial queue so large batches aren't capped at a fixed budget
            # before the no-progress break trips; the no-progress check (below)
            # still terminates promptly when a retry produces nothing new.
            max_retries = max(10, len(pending_ids))
            attempt = 0

            while pending_ids and attempt < max_retries:
                attempt += 1
                if attempt > 1:
                    print(
                        f"  Retry {attempt - 1}: re-running "
                        f"{len(pending_ids)} unfinished tests "
                        f"(skipping {len(known_stalled)} known stalls) ..."
                    )

                with tempfile.TemporaryDirectory(prefix="test262-goccia.") as tmp:
                    temp_dir = Path(tmp)

                    for tid in pending_ids:
                        test_path, metadata = test_meta_by_id[tid]
                        source = test_path.read_text(encoding="utf-8")
                        body = strip_frontmatter(source)
                        description = metadata.get("description", tid)
                        if isinstance(description, dict):
                            description = str(description)
                        negative = metadata.get("negative")
                        includes = metadata.get("includes", [])

                        flags = metadata.get("flags", [])
                        is_async = "async" in flags
                        if is_async and "doneprintHandle.js" not in includes:
                            includes = [*includes, "doneprintHandle.js"]
                        harness_source = build_harness_source(
                            includes, harness_files
                        )

                        if (negative is not None
                                and negative.get("phase") == "runtime"):
                            error_type = negative.get("type", "TypeError")
                            wrapped = wrap_negative_runtime_test(
                                harness_source, body, tid, description,
                                error_type,
                            )
                        else:
                            wrapped = wrap_positive_test(
                                harness_source, body, tid, description,
                                is_async=is_async,
                            )

                        # Hash the tid into a collision-free temp filename.  The
                        # naive replace("/", "__") collapses both `foo/bar` and
                        # `foo__bar` to the same name, which silently overwrites
                        # one wrapper with another and then misattributes the
                        # runner's per-file record to the wrong source test.
                        safe_name = (
                            hashlib.sha1(tid.encode("utf-8")).hexdigest() + ".js"
                        )
                        (temp_dir / safe_name).write_text(
                            wrapped, encoding="utf-8"
                        )

                    json_output = temp_dir / "__results.json"

                    # Per-process GC heap cap.  test262 contains many tests
                    # designed to verify IteratorClose / iterator-cleanup
                    # semantics by running an infinite iterator against an
                    # engine that's expected to stop early.  Any engine bug
                    # in the early-exit path turns those tests into runaway
                    # allocators that OOM the whole runner process — and
                    # because the process holds the entire batch's pending
                    # work, one bad test kills tens of thousands of others.
                    # Capping the GC heap converts a process-killing OOM
                    # into a per-test RangeError that the engine catches
                    # locally; the worker reports the test as failed and
                    # moves on.  2 GiB leaves plenty of headroom for legit
                    # memory-heavy tests on 16 GiB CI runners.  Override via
                    # TEST262_MAX_MEMORY (bytes); set 0 to disable.
                    max_memory_default = 2 * 1024 * 1024 * 1024  # 2 GiB
                    max_memory = int(os.environ.get(
                        "TEST262_MAX_MEMORY", str(max_memory_default)))

                    timeout_ms = timeout * 1000
                    runner_cmd = [
                        str(test_runner),
                        str(temp_dir),
                        # Neither --silent nor --no-progress: forward the
                        # runner's per-file progress lines (`[N/M] file.js`) live
                        # to this script's stdout so CI logs see exactly which
                        # test the runner is processing at any moment.  Without
                        # this, a stall or external SIGTERM mid-run leaves CI
                        # with zero diagnostic output and no way to identify
                        # which test or phase is responsible.
                        f"--output={json_output}",
                        f"--timeout={timeout_ms}",
                        f"--test-timeout={timeout_ms}",
                        f"--describe-timeout={timeout_ms}",
                    ]
                    if max_memory > 0:
                        runner_cmd.append(f"--max-memory={max_memory}")
                    if asi:
                        runner_cmd.append("--asi")
                    if mode != "interpreted":
                        runner_cmd.append(f"--mode={mode}")
                    if compat_var:
                        runner_cmd.append("--compat-var")
                    if compat_function:
                        runner_cmd.append("--compat-function")
                    if unsafe_function_constructor:
                        runner_cmd.append("--unsafe-function-constructor")
                    if jobs is not None:
                        runner_cmd.append(f"--jobs={jobs}")

                    runner_output = ""
                    process_returncode = -1
                    debug_dir = Path(tempfile.gettempdir())
                    # Stream runner output:
                    # - stdout inherits this script's fd, so the runner writes
                    #   directly to our stdout (no Python intermediation).
                    # - stderr is piped and drained on a background thread so we
                    #   can both forward it live to this script's stderr AND
                    #   capture it for fallback diagnostic display when the
                    #   runner exits without a parseable JSON report.
                    captured_stderr_chunks: list[str] = []
                    try:
                        process = subprocess.Popen(
                            runner_cmd,
                            stdout=None,
                            stderr=subprocess.PIPE,
                            bufsize=1,
                            text=True,
                            encoding="utf-8",
                            errors="replace",
                        )

                        def _drain_stderr() -> None:
                            assert process.stderr is not None
                            for line in process.stderr:
                                sys.stderr.write(line)
                                sys.stderr.flush()
                                captured_stderr_chunks.append(line)

                        stderr_thread = threading.Thread(
                            target=_drain_stderr, daemon=True
                        )
                        stderr_thread.start()
                        process.wait()
                        stderr_thread.join(timeout=5)

                        process_returncode = process.returncode
                        runner_output = "".join(captured_stderr_chunks).strip()
                        if process_returncode != 0 and runner_output:
                            try:
                                (debug_dir / "last_runner_stderr.txt").write_text(
                                    runner_output
                                )
                            except OSError:
                                pass
                    except Exception as e:  # noqa: BLE001
                        runner_output = f"Runner invocation failed: {e}"

                    tr_results: dict | None = None
                    if json_output.is_file():
                        raw_json = json_output.read_bytes().decode(
                            "utf-8", errors="replace"
                        )
                        clean_json = re.sub(
                            r"[\x00-\x08\x0b\x0c\x0e-\x1f]", "", raw_json
                        )
                        try:
                            tr_results = json.loads(clean_json)
                        except json.JSONDecodeError as e:
                            # The runner emits a top-level "failedTests"
                            # array that can grow to tens of thousands of
                            # entries when a stalled-worker run produces
                            # many "Worker produced no result" placeholders.
                            # That array is occasionally corrupted in the
                            # serialised JSON (a separate runner-side bug),
                            # but the per-file "results" array we actually
                            # consume is well-formed.  Recover by parsing
                            # each per-file record individually.
                            recovered = _recover_per_file_records(clean_json)
                            if recovered is not None:
                                print(
                                    f"  JSON parse error at {e.pos}; "
                                    f"recovered {len(recovered)} per-file "
                                    f"records via regex fallback"
                                )
                                tr_results = {"results": recovered}
                            else:
                                print(
                                    f"  JSON parse error at {e.pos} of "
                                    f"{len(clean_json)}: {e.msg}"
                                )
                                try:
                                    (debug_dir / "last_runner_json.txt").write_text(
                                        clean_json
                                    )
                                except OSError:
                                    pass
                                tr_results = None
                    else:
                        print(
                            f"  Runner output file missing: {json_output} "
                            f"(exit={process_returncode}, "
                            f"stderr_bytes={len(runner_output)})"
                        )

                    if tr_results is None:
                        print("  TestRunner produced no usable JSON output")
                        if runner_output:
                            for line in runner_output.split("\n")[:10]:
                                print(f"    {line[:200]}")
                        for tid in pending_ids:
                            final_by_id[tid] = {
                                "id": tid,
                                "status": "ERROR",
                                "message": (runner_output[:300]
                                            or "No runner output"),
                            }
                            summary["errors"] += 1
                        pending_ids = []
                        break

                    # Ground truth: per-file records emitted by the runner.
                    file_results_by_safe: dict[str, dict] = {}
                    for fr in tr_results.get("results", []):
                        fpath = fr.get("file", "")
                        key = Path(fpath).name if fpath else ""
                        if key:
                            file_results_by_safe[key] = fr

                    next_pending: list[str] = []
                    attempt_stalls = 0
                    for tid in pending_ids:
                        safe = (
                            hashlib.sha1(tid.encode("utf-8")).hexdigest() + ".js"
                        )
                        fr = file_results_by_safe.get(safe)
                        if fr is None:
                            # Missing per-file record is functionally identical to
                            # the "Worker produced no result" stall: the runner did
                            # not surface an outcome for this file on this attempt.
                            # Requeue so the retry loop can hand it to a fresh
                            # pool; the post-loop tail at L826-835 finalises any
                            # IDs still missing after attempts are exhausted.
                            next_pending.append(tid)
                            continue

                        f_error = fr.get("error", "") or ""
                        f_failed = int(fr.get("failed", 0))
                        f_passed = int(fr.get("passed", 0))
                        f_total = int(fr.get("totalTests", 0))
                        f_failed_tests = fr.get("failedTests", []) or []

                        # Distinguish the two stall categories from real
                        # outcomes.  Stall = the watchdog actually abandoned
                        # the worker on this file; queue-not-drained = a peer
                        # stall left this file untouched, retry will likely
                        # finish it on a fresh pool.
                        is_stalled = "TIMEOUT (worker stalled" in f_error
                        is_no_result = (
                            "Worker produced no result" in f_error
                        )
                        # Cooperative per-test deadlines surface as failed-test
                        # entries containing "TIMEOUT after Xms" rather than as
                        # a worker-stall string.  Without recognising them here
                        # they fall through to the FAIL branch, so the suite's
                        # `timeouts` counter under-reports while `failed`
                        # over-reports.
                        is_test_timeout = any(
                            "TIMEOUT after " in item for item in f_failed_tests
                        )

                        if is_stalled:
                            known_stalled.add(tid)
                            attempt_stalls += 1
                            final_by_id[tid] = {
                                "id": tid,
                                "status": "TIMEOUT",
                                "message": f_error,
                            }
                            summary["timeouts"] += 1
                            continue

                        if is_no_result:
                            next_pending.append(tid)
                            continue

                        if is_test_timeout:
                            final_by_id[tid] = {
                                "id": tid,
                                "status": "TIMEOUT",
                                "message": "; ".join(f_failed_tests) or f_error,
                            }
                            summary["timeouts"] += 1
                            continue

                        if f_failed > 0:
                            final_by_id[tid] = {
                                "id": tid,
                                "status": "FAIL",
                                "message": (
                                    "; ".join(f_failed_tests)
                                    or f_error
                                    or "Test failed"
                                ),
                            }
                            summary["failed"] += 1
                        elif f_passed > 0 and f_total > 0:
                            final_by_id[tid] = {
                                "id": tid,
                                "status": "PASS",
                                "message": "",
                            }
                            summary["passed"] += 1
                        elif f_error:
                            final_by_id[tid] = {
                                "id": tid,
                                "status": "ERROR",
                                "message": f_error,
                            }
                            summary["errors"] += 1
                        else:
                            final_by_id[tid] = {
                                "id": tid,
                                "status": "ERROR",
                                "message": "File ran but registered no tests",
                            }
                            summary["errors"] += 1

                    if attempt_stalls > 0:
                        print(
                            f"  Attempt {attempt}: "
                            f"{attempt_stalls} new native-code stall(s); "
                            f"{len(next_pending)} files still pending"
                        )

                    # Halt when a retry produces no progress — re-running the
                    # exact same set won't break the loop.  Preserve the
                    # exact still-pending set (next_pending) so the post-loop
                    # cleanup records ERROR entries for them in final_by_id;
                    # zeroing pending_ids here would silently drop those IDs.
                    if (len(next_pending) == len(pending_ids)
                            and attempt_stalls == 0):
                        print(
                            f"  No progress on retry; "
                            f"{len(next_pending)} tests remain unprocessed"
                        )
                        pending_ids = next_pending
                        break

                    pending_ids = next_pending

            # Anything still pending after retries is a queue tail no fresh
            # pool managed to drain.  Surface explicitly.
            for tid in pending_ids:
                final_by_id[tid] = {
                    "id": tid,
                    "status": "ERROR",
                    "message": (
                        "Worker produced no result after "
                        f"{attempt} attempts (queue never drained)"
                    ),
                }
                summary["errors"] += 1

        # Append in the original submission order so downstream consumers
        # see results in a stable order.
        for tid in test_ids_in_order:
            results.append(final_by_id[tid])

    # Phase 3: Negative parse-phase tests via ScriptLoader
    if negative_parse_tests:
        print(
            f"Running {len(negative_parse_tests)} negative parse tests "
            f"via GocciaScriptLoader ..."
        )
        for test_path, metadata, test_id in negative_parse_tests:
            source = test_path.read_text(encoding="utf-8")
            body = strip_frontmatter(source)
            negative = metadata.get("negative", {})
            expected_error = negative.get("type", "SyntaxError")

            passed, output, duration = run_negative_parse_test(
                script_loader, body, expected_error, timeout, asi=asi,
                mode=mode, compat_var=compat_var,
                compat_function=compat_function,
                unsafe_function_constructor=unsafe_function_constructor,
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
        help="Prebuilt GocciaTestRunner binary.",
    )
    parser.add_argument(
        "--script-loader",
        type=Path,
        help="Prebuilt GocciaScriptLoader binary.",
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
        default=True,
        help="Enable automatic semicolon insertion (default: True, required for test262).",
    )
    parser.add_argument(
        "--no-asi",
        action="store_true",
        default=False,
        help="Disable automatic semicolon insertion.",
    )
    parser.add_argument(
        "--mode",
        type=str,
        default="bytecode",
        help="Execution mode: interpreted or bytecode (default: bytecode).",
    )
    parser.add_argument(
        "--compat-var",
        action="store_true",
        default=True,
        help="Enable var declarations via --compat-var (default: True, required for test262).",
    )
    parser.add_argument(
        "--no-compat-var",
        action="store_true",
        default=False,
        help="Disable var declaration compatibility.",
    )
    parser.add_argument(
        "--compat-function",
        action="store_true",
        default=True,
        help="Enable `function` keyword support via --compat-function "
             "(default: True, required for test262).",
    )
    parser.add_argument(
        "--no-compat-function",
        action="store_true",
        default=False,
        help="Disable function declaration compatibility.",
    )
    parser.add_argument(
        "--unsafe-function-constructor",
        action="store_true",
        default=True,
        help="Enable the Function constructor via "
             "--unsafe-function-constructor (default: True).",
    )
    parser.add_argument(
        "--no-unsafe-function-constructor",
        action="store_true",
        default=False,
        help="Disable the Function constructor.",
    )
    parser.add_argument(
        "--jobs",
        type=int,
        default=None,
        help="Parallel worker count forwarded to GocciaTestRunner --jobs.",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()

    # --no-asi overrides the default-True --asi
    if args.no_asi:
        args.asi = False
    # --no-compat-var overrides the default-True --compat-var
    if args.no_compat_var:
        args.compat_var = False
    # --no-compat-function overrides the default-True --compat-function
    if args.no_compat_function:
        args.compat_function = False
    # --no-unsafe-function-constructor overrides the default-True flag
    if args.no_unsafe_function_constructor:
        args.unsafe_function_constructor = False

    suite_dir, temp_checkout = ensure_suite_checkout(args.suite_dir)
    test_runner = ensure_binary("GocciaTestRunner", args.test_runner, "testrunner")
    script_loader = ensure_binary("GocciaScriptLoader", args.script_loader, "loader")
    harness_files = load_harness_files()
    categories = tuple(
        c.strip() for c in args.categories.split(",") if c.strip()
    )

    print(f"GocciaTestRunner:    {test_runner}")
    print(f"GocciaScriptLoader:  {script_loader}")
    print(f"Suite:         {suite_dir}")
    print(f"Categories:    {', '.join(categories)}")
    print(f"Timeout:       {args.timeout}s per test")
    if args.filter:
        print(f"Filter:        {args.filter}")
    if args.max_tests:
        print(f"Max tests:     {args.max_tests}")
    print(f"ASI:           {'enabled' if args.asi else 'disabled'}")
    print(f"Compat var:    {'enabled' if args.compat_var else 'disabled'}")
    print(
        f"Compat func:   "
        f"{'enabled' if args.compat_function else 'disabled'}"
    )
    print(
        f"Function():    "
        f"{'enabled' if args.unsafe_function_constructor else 'disabled'}"
    )
    print(f"Mode:          {args.mode}")
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
        mode=args.mode,
        compat_var=args.compat_var,
        compat_function=args.compat_function,
        unsafe_function_constructor=args.unsafe_function_constructor,
        jobs=args.jobs,
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
                for line in r["message"].split("\n")[:10]:
                    print(f"         {line[:200]}")
        print()

    if temp_checkout is not None:
        temp_checkout.cleanup()

    return 1 if s["failed"] > 0 or s["timeouts"] > 0 or s["errors"] > 0 else 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except subprocess.CalledProcessError as error:
        if error.stdout:
            sys.stdout.write(error.stdout)
        if error.stderr:
            sys.stderr.write(error.stderr)
        raise
