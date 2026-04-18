#!/usr/bin/env python3

from __future__ import annotations

import argparse
import json
import subprocess
import sys
import tempfile
from pathlib import Path


SUITE_REPO_URL = "https://github.com/yaml/yaml-test-suite.git"
SUITE_BRANCH = "data"
DEFAULT_TIMEOUT_SECONDS = 5

HARNESS_SOURCE = """program GocciaYAMLCheck;

{$I REPO_ROOT/source/units/Goccia.inc}

uses
  SysUtils,
  Classes,

  Goccia.GarbageCollector,

  Goccia.Values.Primitives,
  Goccia.Values.ArrayValue,
  Goccia.YAML;

var
  ExitCode: Integer;
  Documents: TGocciaArrayValue;
  Parser: TGocciaYAMLParser;
  Source: TStringList;
begin
  if ParamCount <> 1 then
    Halt(2);

  ExitCode := 0;
  TGarbageCollector.Initialize;
  PinPrimitiveSingletons;

  Source := TStringList.Create;
  Parser := TGocciaYAMLParser.Create;
  try
    try
      Source.LoadFromFile(ParamStr(1));
      Documents := Parser.ParseDocuments(Source.Text);
      Documents.Free;
    except
      on E: Exception do
      begin
        Writeln(E.Message);
        ExitCode := 1;
      end;
    end;
  finally
    Parser.Free;
    Source.Free;
    TGarbageCollector.Shutdown;
  end;

  Halt(ExitCode);
end.
"""


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

  temp_dir = tempfile.TemporaryDirectory(prefix="yaml-test-suite.")
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
  harness_path = build_dir / "goccia_yaml_check.pas"
  harness_path.write_text(HARNESS_SOURCE.replace("REPO_ROOT", str(repo_root)))

  run(
    [
      "fpc",
      "@config.cfg",
      "-Fusource/units",
      "-Fusource/shared",
      f"-FU{build_dir}",
      f"-FE{build_dir}",
      str(harness_path),
    ],
    cwd=repo_root,
  )
  return build_dir / "goccia_yaml_check"


def evaluate_suite(harness_path: Path, suite_dir: Path, timeout_seconds: int) -> dict:
  results = []
  summary = {
    "total": 0,
    "passed": 0,
    "failed": 0,
    "false_accepts": 0,
    "false_rejects": 0,
    "timeouts": 0,
  }

  case_dirs = sorted({path.parent for path in suite_dir.rglob("in.yaml")})
  for case_dir in case_dirs:
    case_id = str(case_dir.relative_to(suite_dir))
    expected_fail = (case_dir / "error").exists()
    summary["total"] += 1

    try:
      process = subprocess.run(
        [str(harness_path), str(case_dir / "in.yaml")],
        text=True,
        capture_output=True,
        timeout=timeout_seconds,
      )
      timed_out = False
      actual_fail = process.returncode != 0
      message = (process.stdout + process.stderr).strip()
    except subprocess.TimeoutExpired:
      timed_out = True
      actual_fail = True
      message = "timeout"

    ok = (expected_fail == actual_fail) and not timed_out
    if timed_out:
      summary["timeouts"] += 1
    elif ok:
      summary["passed"] += 1
    else:
      summary["failed"] += 1
      if expected_fail and not actual_fail:
        summary["false_accepts"] += 1
      elif (not expected_fail) and actual_fail:
        summary["false_rejects"] += 1

    results.append(
      {
        "id": case_id,
        "expected_fail": expected_fail,
        "actual_fail": actual_fail,
        "ok": ok,
        "timed_out": timed_out,
        "message": message,
      }
    )

  return {"summary": summary, "results": results}


def parse_args() -> argparse.Namespace:
  parser = argparse.ArgumentParser(
    description="Run Goccia's YAML parser against the official yaml-test-suite data branch."
  )
  parser.add_argument(
    "--suite-dir",
    type=Path,
    help="Existing checkout of yaml-test-suite data branch. If omitted, the script clones it to a temporary directory.",
  )
  parser.add_argument(
    "--build-dir",
    type=Path,
    default=Path(tempfile.gettempdir()) / "goccia_yaml_suite_build",
    help="Directory for the temporary harness source and binary.",
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
  harness_path = compile_harness(repo_root, args.build_dir.resolve())
  report = evaluate_suite(harness_path, suite_dir.resolve(), args.timeout)

  if args.output is not None:
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(report, indent=2) + "\n")

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
