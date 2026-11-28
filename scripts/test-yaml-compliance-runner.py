#!/usr/bin/env python3
"""Exercise GocciaYAMLComplianceRunner against small prepared suites.

Run after building it: python3 scripts/test-yaml-compliance-runner.py
Each check builds a throwaway git checkout shaped like the yaml-test-suite
data branch, with its own pin file, and asserts the runner's exit status and
the outcome it reports for each case.
"""
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile

REPO = Path(__file__).resolve().parents[1]
RUNNER = REPO / "build" / "GocciaYAMLComplianceRunner"
if not RUNNER.exists():
    RUNNER = RUNNER.with_suffix(".exe")
if not RUNNER.exists():
    raise SystemExit("build GocciaYAMLComplianceRunner first: ./build.pas yamlcompliancerunner")

# case ID -> files; "error" marks a case the parser must reject
def events(*lines):
    return "\n".join(("+STR",) + lines + ("-STR",)) + "\n"


PASSING_CASES = {
    "AAAA": {"in.yaml": "a: 1\nb: [x, y]\n", "in.json": '{"b": ["x", "y"], "a": 1}\n',
             "test.event": events("+DOC", "+MAP", "=VAL :a", "=VAL :1", "=VAL :b", "+SEQ []",
                                  "=VAL :x", "=VAL :y", "-SEQ", "-MAP", "-DOC")},
    "BBBB": {"in.yaml": "[a\n", "error": ""},
    "CCCC/00": {"in.yaml": "--- 1\n--- two\n", "in.json": '1\n"two"\n',
                "test.event": events("+DOC ---", "=VAL :1", "-DOC", "+DOC ---", "=VAL :two", "-DOC")},
    "DDDD": {"in.yaml": "? [a, b]\n: c\n",
             "test.event": events("+DOC", "+MAP", "+SEQ []", "=VAL :a", "=VAL :b", "-SEQ",
                                  "=VAL :c", "-MAP", "-DOC")},
}
A_ONE = events("+DOC", "+MAP", "=VAL :a", "=VAL :1", "-MAP", "-DOC")
FAILING_CASES = {
    "MISM": {"in.yaml": "a: 1\n", "in.json": '{"a": 2}\n', "test.event": A_ONE},
    "EVNT": {"in.yaml": "a: 1\n", "in.json": '{"a": 1}\n', "test.event": A_ONE.replace(":1", ":2")},
    "FACC": {"in.yaml": "a: 1\n", "error": ""},
    "FREJ": {"in.yaml": "[a\n", "in.json": '["a"]\n', "test.event": A_ONE},
    # the events parse, but the documents do not compose: the event parser
    # must find what an error case gets wrong
    "VALS": {"in.yaml": "!!int nope\n", "error": ""},
}
EXPECTED_FAILURES = {"MISM": "mismatch", "EVNT": "mismatch", "FACC": "false_accept",
                     "FREJ": "false_reject", "VALS": "false_accept"}


def make_suite(root, cases):
    suite = root / "suite"
    suite.mkdir()
    for case_id, files in cases.items():
        directory = suite / case_id
        directory.mkdir(parents=True)
        for name, text in files.items():
            (directory / name).write_text(text)
    # the data branch indexes its cases through symlinks, which the runner
    # must not run again
    (suite / "name").mkdir()
    os.symlink(suite / "AAAA", suite / "name" / "some-case", target_is_directory=True)
    git = ["git", "-C", str(suite), "-c", "user.name=test", "-c", "user.email=test@example.com"]
    subprocess.run(["git", "init", "-q", str(suite)], check=True)
    subprocess.run(git + ["add", "-A"], check=True)
    subprocess.run(git + ["commit", "-q", "-m", "suite"], check=True)
    revision = subprocess.run(git + ["rev-parse", "HEAD"], check=True,
                              capture_output=True, text=True).stdout.strip()
    (root / "pin").write_text(revision + "\n")
    return suite


def run(root, suite, pin=None, extra=()):
    report = root / "report.json"
    if report.exists():
        report.unlink()
    process = subprocess.run(
        [str(RUNNER), f"--suite-dir={suite}", f"--pin-file={pin or root / 'pin'}",
         f"--output={report}", "--jobs=2", *extra],
        capture_output=True, text=True, timeout=120)
    outcomes = {}
    if report.exists():
        data = json.loads(report.read_text())
        outcomes = {case["id"]: case["outcome"] for case in data["cases"]}
    return process.returncode, outcomes, process.stdout + process.stderr


def expect(condition, message, output):
    if not condition:
        print(output)
        raise SystemExit(f"FAIL: {message}")


with tempfile.TemporaryDirectory(prefix="goccia-yaml-runner-check-") as directory:
    root = Path(directory) / "passing"
    root.mkdir()
    suite = make_suite(root, PASSING_CASES)
    code, outcomes, output = run(root, suite)
    expect(code == 0, f"a suite that passes exits 0, got {code}", output)
    expect(set(outcomes) == set(PASSING_CASES),
           f"the symlink index is skipped and every case runs once, got {sorted(outcomes)}", output)
    expect(all(outcomes[case] == "passed" for case in PASSING_CASES),
           f"each case passes, got {outcomes}", output)

    (root / "not-a-pin").write_text("not a revision\n")
    code, outcomes, output = run(root, suite, pin=root / "not-a-pin")
    expect(code == 2, f"a pin that is no SHA is an unusable invocation, got {code}", output)

    code, outcomes, output = run(root, suite, extra=[f"--known-failures={root / 'pin'}"])
    expect(code == 2, f"the removed --known-failures option is an unusable invocation, got {code}", output)
    code, outcomes, output = run(root, suite, extra=["--known-failures"])
    expect(code == 2, f"a bare --known-failures is an unusable invocation, got {code}", output)

    root = Path(directory) / "failing"
    root.mkdir()
    suite = make_suite(root, FAILING_CASES)
    code, outcomes, output = run(root, suite)
    expect(code == 1, f"failing cases fail the run, got {code}", output)
    expect(outcomes == EXPECTED_FAILURES, f"each failure is classified, got {outcomes}", output)

print("YAML compliance runner checks passed")
