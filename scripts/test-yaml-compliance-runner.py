#!/usr/bin/env python3
"""Exercise GocciaYAMLComplianceRunner against small prepared suites.

Run after building it: python3 scripts/test-yaml-compliance-runner.py
Each check builds a throwaway git checkout shaped like the yaml-test-suite
data branch, with its own pin and known-failures files, and asserts the
runner's exit status and the outcome it reports for each case.
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
PASSING_CASES = {
    "AAAA": {"in.yaml": "a: 1\nb: [x, y]\n", "in.json": '{"b": ["x", "y"], "a": 1}\n'},
    "BBBB": {"in.yaml": "[a\n", "error": ""},
    "CCCC/00": {"in.yaml": "--- 1\n--- two\n", "in.json": '1\n"two"\n'},
    "DDDD": {"in.yaml": "? [a, b]\n: c\n"},
}
FAILING_CASES = {
    "MISM": {"in.yaml": "a: 1\n", "in.json": '{"a": 2}\n'},
    "FACC": {"in.yaml": "a: 1\n", "error": ""},
    "FREJ": {"in.yaml": "[a\n", "in.json": '["a"]\n'},
}


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


def run(root, suite, known_failures, pin=None):
    (root / "known-failures").write_text("".join(f"{case}  # listed\n" for case in known_failures))
    report = root / "report.json"
    if report.exists():
        report.unlink()
    process = subprocess.run(
        [str(RUNNER), f"--suite-dir={suite}", f"--pin-file={pin or root / 'pin'}",
         f"--known-failures={root / 'known-failures'}", f"--output={report}", "--jobs=2"],
        capture_output=True, text=True, timeout=120)
    outcomes = {}
    if report.exists():
        data = json.loads(report.read_text())
        outcomes = {case["id"]: case["outcome"] for case in data["cases"]}
        outcomes["#knownFailures"] = data["knownFailures"]
    return process.returncode, outcomes, process.stdout + process.stderr


def expect(condition, message, output):
    if not condition:
        print(output)
        raise SystemExit(f"FAIL: {message}")


with tempfile.TemporaryDirectory(prefix="goccia-yaml-runner-check-") as directory:
    root = Path(directory) / "passing"
    root.mkdir()
    suite = make_suite(root, PASSING_CASES)
    code, outcomes, output = run(root, suite, [])
    expect(code == 0, f"a suite that passes exits 0, got {code}", output)
    expect(set(outcomes) - {"#knownFailures"} == set(PASSING_CASES),
           f"the symlink index is skipped and every case runs once, got {sorted(outcomes)}", output)
    expect(all(outcomes[case] == "passed" for case in PASSING_CASES),
           f"each case passes, got {outcomes}", output)

    code, outcomes, output = run(root, suite, ["AAAA"])
    expect(code == 1, f"a listed case that passes fails the run, got {code}", output)
    expect(outcomes["#knownFailures"]["nowPassing"] == ["AAAA"],
           f"the passing listed case is reported, got {outcomes['#knownFailures']}", output)

    code, outcomes, output = run(root, suite, ["ZZZZ"])
    expect(code == 1, f"a listed case the suite lacks fails the run, got {code}", output)
    expect(outcomes["#knownFailures"]["notInSuite"] == ["ZZZZ"],
           f"the missing listed case is reported, got {outcomes['#knownFailures']}", output)

    code, outcomes, output = run(root, suite, [], pin=root / "known-failures")
    expect(code == 2, f"a pin that is no SHA is an unusable invocation, got {code}", output)

    root = Path(directory) / "failing"
    root.mkdir()
    suite = make_suite(root, FAILING_CASES)
    code, outcomes, output = run(root, suite, [])
    expect(code == 1, f"failing cases fail the run, got {code}", output)
    expect(outcomes.get("MISM") == "mismatch", f"a different value is a mismatch, got {outcomes}", output)
    expect(outcomes.get("FACC") == "false_accept", f"an accepted error case is a false accept, got {outcomes}", output)
    expect(outcomes.get("FREJ") == "false_reject", f"a rejected valid case is a false reject, got {outcomes}", output)
    expect(sorted(outcomes["#knownFailures"]["unexpectedFailures"]) == sorted(FAILING_CASES),
           f"each unlisted failure is reported, got {outcomes['#knownFailures']}", output)

    code, outcomes, output = run(root, suite, list(FAILING_CASES))
    expect(code == 0, f"listed failures let the run pass, got {code}", output)

print("YAML compliance runner checks passed")
