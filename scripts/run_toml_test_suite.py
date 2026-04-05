#!/usr/bin/env python3

from __future__ import annotations

import argparse
import json
import math
import re
import subprocess
import sys
import tempfile
from pathlib import Path


SUITE_REPO_URL = "https://github.com/toml-lang/toml-test.git"
SUITE_VERSION = "1.1.0"
SUITE_BRANCH = "main"
SUITE_FILE_LIST = f"tests/files-toml-{SUITE_VERSION}"
DEFAULT_TIMEOUT_SECONDS = 5
HARNESS_SOURCE_PATH = Path("scripts/GocciaTOMLCheck.dpr")
TIME_TEXT_PATTERN = re.compile(r"^(?P<prefix>\d{2}:\d{2}:\d{2})(?P<fraction>\.\d+)?(?P<suffix>)$")
DATETIME_TEXT_PATTERN = re.compile(
  r"^(?P<prefix>\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2})(?P<fraction>\.\d+)?(?P<suffix>[+-]\d{2}:\d{2})?$"
)


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

  temp_dir = tempfile.TemporaryDirectory(prefix="toml-test-suite.")
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
  harness_name = "GocciaTOMLCheck.exe" if sys.platform.startswith("win") else "GocciaTOMLCheck"
  return build_dir / harness_name


def normalize_datetime_text(kind: str, value: str) -> str:
  value = value.replace(" ", "T").replace("t", "T").replace("z", "Z")
  if value.endswith("Z"):
    value = value[:-1] + "+00:00"

  if kind == "date-local":
    return value

  pattern = TIME_TEXT_PATTERN if kind == "time-local" else DATETIME_TEXT_PATTERN
  match = pattern.match(value)
  if match is None:
    return value

  fraction = match.group("fraction") or ""
  if fraction:
    fraction = fraction.rstrip("0")
    if fraction == ".":
      fraction = ""

  return match.group("prefix") + fraction + (match.group("suffix") or "")


def is_tagged_scalar(node) -> bool:
  return (
    isinstance(node, dict)
    and set(node.keys()) == {"type", "value"}
    and isinstance(node.get("type"), str)
    and isinstance(node.get("value"), str)
  )


def compare_tagged_json(want, have, path="") -> tuple[bool, str]:
  if isinstance(want, dict):
    if not isinstance(have, dict):
      return False, f"{path or '<root>'}: expected object, got {type(have).__name__}"

    if is_tagged_scalar(want):
      if not is_tagged_scalar(have):
        return False, f"{path or '<root>'}: malformed tagged value"

      want_type = want["type"]
      have_type = have["type"]
      if want_type != have_type:
        return False, f"{path or '<root>'}: expected type {want_type}, got {have_type}"

      want_value = want["value"]
      have_value = have["value"]

      if want_type == "float":
        want_lower = want_value.lower()
        have_lower = have_value.lower()
        if want_lower.endswith("nan") or have_lower.endswith("nan"):
          if want_lower.lstrip("+-") != have_lower.lstrip("+-"):
            return False, f"{path or '<root>'}: expected float {want_value}, got {have_value}"
          return True, ""
        want_float = float(want_lower)
        have_float = float(have_lower)
        if want_float != have_float:
          return False, f"{path or '<root>'}: expected float {want_value}, got {have_value}"
        if want_float == 0.0 and math.copysign(1.0, want_float) != math.copysign(1.0, have_float):
          return False, f"{path or '<root>'}: expected float {want_value}, got {have_value}"
        return True, ""

      if want_type in {"datetime", "datetime-local", "date-local", "time-local"}:
        if normalize_datetime_text(want_type, want_value) != normalize_datetime_text(want_type, have_value):
          return False, f"{path or '<root>'}: expected datetime {want_value}, got {have_value}"
        return True, ""

      if want_type == "bool":
        if want_value.lower() != str(have_value).lower():
          return False, f"{path or '<root>'}: expected bool {want_value}, got {have_value}"
        return True, ""

      if want_value != have_value:
        return False, f"{path or '<root>'}: expected {want_value}, got {have_value}"
      return True, ""

    if set(want.keys()) != set(have.keys()):
      return False, f"{path or '<root>'}: object keys differ"

    for key in sorted(want.keys()):
      ok, message = compare_tagged_json(want[key], have[key], f"{path}.{key}" if path else key)
      if not ok:
        return False, message
    return True, ""

  if isinstance(want, list):
    if not isinstance(have, list):
      return False, f"{path or '<root>'}: expected array, got {type(have).__name__}"
    if len(want) != len(have):
      return False, f"{path or '<root>'}: array lengths differ"
    for index, (want_item, have_item) in enumerate(zip(want, have)):
      ok, message = compare_tagged_json(want_item, have_item, f"{path}[{index}]")
      if not ok:
        return False, message
    return True, ""

  if want != have:
    return False, f"{path or '<root>'}: expected {want}, got {have}"
  return True, ""


def decode_toml_input(case_path: Path) -> tuple[bool, str]:
  raw = case_path.read_bytes()
  try:
    text = raw.decode("utf-8")
  except UnicodeDecodeError as error:
    return False, f"invalid UTF-8: {error}"

  if "\r" in text.replace("\r\n", ""):
    return False, "bare carriage return is not allowed"

  return True, text


def evaluate_suite(harness_path: Path, suite_dir: Path, timeout_seconds: int) -> dict:
  file_list_path = suite_dir / SUITE_FILE_LIST
  rel_paths = [
    line.strip()
    for line in file_list_path.read_text(encoding="utf-8").splitlines()
    if line.strip() and not line.startswith("#") and line.endswith(".toml")
  ]

  results = []
  summary = {
    "suite_version": SUITE_VERSION,
    "total": 0,
    "passed": 0,
    "failed": 0,
    "false_accepts": 0,
    "false_rejects": 0,
    "valid_mismatches": 0,
    "timeouts": 0,
  }

  for rel_path in rel_paths:
    case_path = suite_dir / "tests" / rel_path
    is_valid = rel_path.startswith("valid/")
    expected_json_path = case_path.with_suffix(".json")
    summary["total"] += 1

    utf8_ok, utf8_message = decode_toml_input(case_path)
    if not utf8_ok:
      actual_fail = True
      mismatch = False
      ok = not is_valid
      if ok:
        summary["passed"] += 1
      else:
        summary["failed"] += 1
        summary["false_rejects"] += 1
      results.append(
        {
          "id": rel_path,
          "is_valid": is_valid,
          "actual_fail": actual_fail,
          "ok": ok,
          "timed_out": False,
          "mismatch": mismatch,
          "message": utf8_message,
        }
      )
      continue

    try:
      process = subprocess.run(
        [str(harness_path), str(case_path)],
        capture_output=True,
        timeout=timeout_seconds,
      )
      timed_out = False
      actual_fail = process.returncode != 0
      stdout = process.stdout.decode("utf-8", errors="replace").strip()
      stderr = process.stderr.decode("utf-8", errors="replace").strip()
      message = "\n".join(part for part in [stdout, stderr] if part)
    except subprocess.TimeoutExpired:
      timed_out = True
      actual_fail = True
      stdout = ""
      message = "timeout"

    ok = False
    mismatch = False
    if timed_out:
      summary["timeouts"] += 1
    elif is_valid:
      if not actual_fail:
        try:
          actual_json = json.loads(stdout or "null")
        except json.JSONDecodeError as error:
          ok = False
          message = f"invalid decoder JSON: {error}"
        else:
          try:
            expected_json = json.loads(expected_json_path.read_text(encoding="utf-8"))
          except (OSError, json.JSONDecodeError) as error:
            ok = False
            message = f"invalid expected JSON fixture: {error}"
          else:
            ok, message = compare_tagged_json(expected_json, actual_json)
            mismatch = not ok
      else:
        ok = False
      if not ok:
        summary["failed"] += 1
        summary["false_rejects"] += 1
        if mismatch:
          summary["valid_mismatches"] += 1
          if not message:
            message = "decoded JSON mismatch"
    else:
      ok = actual_fail
      if not ok:
        summary["failed"] += 1
        summary["false_accepts"] += 1

    if ok:
      summary["passed"] += 1

    results.append(
      {
        "id": rel_path,
        "is_valid": is_valid,
        "actual_fail": actual_fail,
        "ok": ok,
        "timed_out": timed_out,
        "mismatch": mismatch,
        "message": message,
      }
    )

  return {"summary": summary, "results": results}


def parse_args() -> argparse.Namespace:
  parser = argparse.ArgumentParser(
    description="Run Goccia's TOML parser against the official toml-test TOML 1.1.0 suite."
  )
  parser.add_argument(
    "--suite-dir",
    type=Path,
    help="Existing checkout of toml-test. If omitted, the script clones it to a temporary directory.",
  )
  parser.add_argument(
    "--build-dir",
    type=Path,
    default=Path(tempfile.gettempdir()) / "goccia_toml_suite_build",
    help="Directory for the temporary harness source and binary.",
  )
  parser.add_argument(
    "--harness",
    type=Path,
    help="Existing TOML decoder harness binary. If omitted, the script compiles scripts/GocciaTOMLCheck.dpr.",
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

  suite_dir, temp_checkout = ensure_suite_checkout(args.suite_dir)
  if args.harness is not None:
    harness_path = args.harness.resolve()
    if not harness_path.is_file():
      raise FileNotFoundError(f"TOML harness not found: {harness_path}")
  else:
    harness_path = compile_harness(repo_root, args.build_dir.resolve())
  report = evaluate_suite(harness_path, suite_dir.resolve(), args.timeout)

  if args.output is not None:
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")

  print(json.dumps(report["summary"], indent=2))
  if args.output is not None:
    print(args.output.resolve())

  if temp_checkout is not None:
    temp_checkout.cleanup()

  summary = report["summary"]
  if summary["failed"] > 0 or summary["timeouts"] > 0:
    return 1

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
