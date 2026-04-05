#!/usr/bin/env python3

from __future__ import annotations

import argparse
import json
import math
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path


SUITE_REPO_URL = "https://github.com/json5/json5.git"
SUITE_BRANCH = "main"
DEFAULT_TIMEOUT_SECONDS = 5
HARNESS_SOURCE_PATH = Path("scripts/GocciaJSON5Check.dpr")
CASE_EXTRACTOR_PATH = Path("scripts/extract_json5_cases.js")


def run(command: list[str], cwd: Path | None = None) -> subprocess.CompletedProcess[str]:
  return subprocess.run(
    command,
    cwd=str(cwd) if cwd else None,
    text=True,
    capture_output=True,
    check=True,
  )


def ensure_suite_checkout(suite_dir: Path | None) -> tuple[Path, tempfile.TemporaryDirectory[str] | None]:
  if suite_dir is not None:
    return suite_dir.resolve(), None

  temp_dir = tempfile.TemporaryDirectory(prefix="json5-test-suite.")
  checkout_dir = Path(temp_dir.name) / "repo"
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
  return checkout_dir, temp_dir


def find_node_executable(explicit: str | None) -> str:
  if explicit:
    return explicit

  for candidate in ["node", "nodejs"]:
    resolved = shutil.which(candidate)
    if resolved:
      return resolved

  raise FileNotFoundError("Node.js executable not found. Install `node` or pass --node.")


def compile_harness(repo_root: Path, build_dir: Path) -> Path:
  build_dir.mkdir(parents=True, exist_ok=True)
  harness_source = repo_root / HARNESS_SOURCE_PATH

  run(
    [
      "fpc",
      "@config.cfg",
      "-Fuunits",
      "-Fu.",
      f"-FU{build_dir}",
      f"-FE{build_dir}",
      str(harness_source),
    ],
    cwd=repo_root,
  )

  harness_name = "GocciaJSON5Check.exe" if sys.platform.startswith("win") else "GocciaJSON5Check"
  return build_dir / harness_name


def load_cases(repo_root: Path, suite_dir: Path, node_executable: str) -> list[dict]:
  extractor = repo_root / CASE_EXTRACTOR_PATH
  process = run([node_executable, str(extractor), str(suite_dir)])
  payload = json.loads(process.stdout)
  return payload["cases"]


def compare_values(want: dict, have: dict, path: str = "<root>") -> tuple[bool, str]:
  if want.get("type") != have.get("type"):
    return False, f"{path}: expected type {want.get('type')}, got {have.get('type')}"

  value_type = want["type"]
  if value_type == "null":
    return True, ""

  if value_type in {"boolean", "string"}:
    if want.get("value") != have.get("value"):
      return False, f"{path}: expected {want.get('value')!r}, got {have.get('value')!r}"
    return True, ""

  if value_type == "number":
    want_value = want["value"]
    have_value = have["value"]
    if want_value in {"NaN", "Infinity", "-Infinity", "-0"} or have_value in {"NaN", "Infinity", "-Infinity", "-0"}:
      if want_value != have_value:
        return False, f"{path}: expected number {want_value}, got {have_value}"
      return True, ""

    if float(want_value) != float(have_value):
      return False, f"{path}: expected number {want_value}, got {have_value}"
    if math.copysign(1.0, float(want_value)) != math.copysign(1.0, float(have_value)):
      return False, f"{path}: expected signed number {want_value}, got {have_value}"
    return True, ""

  if value_type == "array":
    want_items = want["items"]
    have_items = have["items"]
    if len(want_items) != len(have_items):
      return False, f"{path}: expected array length {len(want_items)}, got {len(have_items)}"
    for index, (want_item, have_item) in enumerate(zip(want_items, have_items)):
      ok, message = compare_values(want_item, have_item, f"{path}[{index}]")
      if not ok:
        return False, message
    return True, ""

  if value_type == "object":
    want_entries = want["entries"]
    have_entries = have["entries"]
    if len(want_entries) != len(have_entries):
      return False, f"{path}: expected {len(want_entries)} object entries, got {len(have_entries)}"
    for index, (want_entry, have_entry) in enumerate(zip(want_entries, have_entries)):
      if want_entry["key"] != have_entry["key"]:
        return False, (
          f"{path}: expected key {want_entry['key']!r} at index {index}, "
          f"got {have_entry['key']!r}"
        )
      ok, message = compare_values(
        want_entry["value"],
        have_entry["value"],
        f"{path}.{want_entry['key']}",
      )
      if not ok:
        return False, message
    return True, ""

  return False, f"{path}: unsupported tagged value type {value_type}"


def evaluate_suite(harness_path: Path, cases: list[dict], timeout_seconds: int) -> dict:
  results = []
  summary = {
    "total": 0,
    "passed": 0,
    "failed": 0,
    "false_accepts": 0,
    "false_rejects": 0,
    "valid_mismatches": 0,
    "timeouts": 0,
  }

  with tempfile.TemporaryDirectory(prefix="json5-suite-cases.") as case_dir_name:
    case_dir = Path(case_dir_name)

    for index, case in enumerate(cases):
      summary["total"] += 1
      case_path = case_dir / f"case_{index}.json5"
      case_path.write_text(case["source"], encoding="utf-8")

      try:
        process = subprocess.run(
          [str(harness_path), str(case_path)],
          text=True,
          capture_output=True,
          timeout=timeout_seconds,
        )
        timed_out = False
      except subprocess.TimeoutExpired:
        timed_out = True
        process = None

      if timed_out:
        summary["failed"] += 1
        summary["timeouts"] += 1
        results.append(
          {
            "id": case["id"],
            "valid": case["valid"],
            "ok": False,
            "timed_out": True,
            "message": "timeout",
          }
        )
        continue

      stdout = process.stdout.strip()
      stderr = process.stderr.strip()
      actual_fail = process.returncode != 0
      message = "\n".join(part for part in [stdout, stderr] if part)

      if not case["valid"]:
        ok = actual_fail
        if ok:
          summary["passed"] += 1
        else:
          summary["failed"] += 1
          summary["false_accepts"] += 1
        results.append(
          {
            "id": case["id"],
            "valid": False,
            "ok": ok,
            "timed_out": False,
            "message": message,
          }
        )
        continue

      if actual_fail:
        summary["failed"] += 1
        summary["false_rejects"] += 1
        results.append(
          {
            "id": case["id"],
            "valid": True,
            "ok": False,
            "timed_out": False,
            "message": message,
          }
        )
        continue

      try:
        actual_value = json.loads(stdout)
      except json.JSONDecodeError as error:
        summary["failed"] += 1
        summary["valid_mismatches"] += 1
        results.append(
          {
            "id": case["id"],
            "valid": True,
            "ok": False,
            "timed_out": False,
            "message": f"invalid harness JSON: {error}",
          }
        )
        continue

      ok, mismatch_message = compare_values(case["expected"], actual_value)
      if ok:
        summary["passed"] += 1
      else:
        summary["failed"] += 1
        summary["valid_mismatches"] += 1

      results.append(
        {
          "id": case["id"],
          "valid": True,
          "ok": ok,
          "timed_out": False,
          "message": mismatch_message,
        }
      )

  return {"summary": summary, "results": results}


def parse_args() -> argparse.Namespace:
  parser = argparse.ArgumentParser(
    description="Run Goccia's JSON5 parser against the official json5/json5 parser test corpus."
  )
  parser.add_argument(
    "--suite-dir",
    type=Path,
    help="Existing checkout of json5/json5. If omitted, the script clones it to a temporary directory.",
  )
  parser.add_argument(
    "--build-dir",
    type=Path,
    default=Path(tempfile.gettempdir()) / "goccia_json5_suite_build",
    help="Directory for the temporary JSON5 harness build artifacts.",
  )
  parser.add_argument(
    "--harness",
    type=Path,
    help="Optional prebuilt GocciaJSON5Check harness. If omitted, the script compiles scripts/GocciaJSON5Check.dpr.",
  )
  parser.add_argument(
    "--node",
    help="Optional Node.js executable path. Defaults to `node`/`nodejs` from PATH.",
  )
  parser.add_argument(
    "--output",
    type=Path,
    help="Optional JSON output path for the full summary and per-case results.",
  )
  parser.add_argument(
    "--timeout",
    type=int,
    default=DEFAULT_TIMEOUT_SECONDS,
    help=f"Per-case timeout in seconds. Default: {DEFAULT_TIMEOUT_SECONDS}.",
  )
  return parser.parse_args()


def main() -> int:
  args = parse_args()
  repo_root = Path(__file__).resolve().parent.parent
  node_executable = find_node_executable(args.node)

  suite_dir, temp_checkout = ensure_suite_checkout(args.suite_dir)
  if args.harness is not None:
    harness_path = args.harness.resolve()
  else:
    harness_path = compile_harness(repo_root, args.build_dir.resolve())

  cases = load_cases(repo_root, suite_dir, node_executable)
  report = evaluate_suite(harness_path, cases, args.timeout)

  if args.output is not None:
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")

  print(json.dumps(report["summary"], indent=2))
  if args.output is not None:
    print(args.output.resolve())

  if temp_checkout is not None:
    temp_checkout.cleanup()

  return 0


if __name__ == "__main__":
  try:
    raise SystemExit(main())
  except subprocess.CalledProcessError as error:
    if error.stdout:
      sys.stdout.write(error.stdout)
    if error.stderr:
      sys.stderr.write(error.stderr)
    raise
