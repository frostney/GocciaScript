#!/usr/bin/env python3

from __future__ import annotations

import argparse
import base64
import hashlib
import io
import json
import os
import re
import subprocess
import sys
import tarfile
import tempfile
import urllib.error
import urllib.parse
import urllib.request
from pathlib import Path
from typing import Any


HARNESS_DIRECTORY = Path("scripts/es_toolkit_harness")
DEFAULT_MANIFEST = HARNESS_DIRECTORY / "manifest.json"
RESULT_MARKER = "GocciaEsToolkitResult:"
ENVIRONMENT_MARKER = "GocciaEsToolkitEnvironment:"
MODES = ("interpreted", "bytecode")
NPM_REGISTRY_HOST = "registry.npmjs.org"


def parse_args() -> argparse.Namespace:
  parser = argparse.ArgumentParser(
    description="Run bounded, pinned es-toolkit compatibility probes in both Goccia execution modes."
  )
  parser.add_argument("--manifest", type=Path, default=DEFAULT_MANIFEST)
  parser.add_argument("--package-dir", type=Path, help="Use an already extracted es-toolkit npm package directory.")
  parser.add_argument("--tarball-cache", type=Path, help="Use or populate a verified es-toolkit npm tarball cache.")
  parser.add_argument("--goccia", type=Path, default=Path("build/GocciaScriptLoader"))
  parser.add_argument("--output", type=Path, help="Write the normalized JSON report to this path.")
  parser.add_argument("--timeout-seconds", type=int, default=30)
  return parser.parse_args()


def load_manifest(path: Path) -> dict[str, Any]:
  manifest = json.loads(path.read_text(encoding="utf-8"))
  if manifest.get("schemaVersion") != 1:
    raise ValueError("es-toolkit manifest schemaVersion must be 1")
  return manifest


def validate_tarball_url(url: str) -> None:
  parsed = urllib.parse.urlsplit(url)
  if parsed.scheme != "https" or parsed.hostname != NPM_REGISTRY_HOST:
    raise ValueError(f"es-toolkit tarball URL must use https://{NPM_REGISTRY_HOST}")


class NpmRegistryRedirectHandler(urllib.request.HTTPRedirectHandler):
  def redirect_request(self, request, file_pointer, code, message, headers, new_url):
    validate_tarball_url(new_url)
    return super().redirect_request(request, file_pointer, code, message, headers, new_url)


def download_tarball(url: str) -> bytes:
  validate_tarball_url(url)
  opener = urllib.request.build_opener(NpmRegistryRedirectHandler())
  with opener.open(url, timeout=30) as response:
    return response.read()


def verify_integrity(data: bytes, integrity: str) -> None:
  algorithm, expected = integrity.split("-", 1)
  if algorithm != "sha512":
    raise ValueError(f"unsupported npm integrity algorithm: {algorithm}")
  actual = base64.b64encode(hashlib.sha512(data).digest()).decode("ascii")
  if actual != expected:
    raise ValueError("downloaded es-toolkit package does not match the pinned npm integrity")


def extract_verified_package(data: bytes, destination: Path) -> Path:
  destination_root = destination.resolve()
  with tarfile.open(fileobj=io.BytesIO(data), mode="r:gz") as archive:
    for member in archive.getmembers():
      member_path = (destination / member.name).resolve()
      if os.path.commonpath([str(destination_root), str(member_path)]) != str(destination_root):
        raise ValueError(f"unsafe path in es-toolkit package: {member.name}")
      if not (member.isfile() or member.isdir()):
        raise ValueError(f"unsupported archive entry in es-toolkit package: {member.name}")
    archive.extractall(destination, filter="data")
  return destination / "package"


def prepare_package(
  manifest: dict[str, Any], package_dir: Path | None, tarball_cache: Path | None
) -> tuple[Path, tempfile.TemporaryDirectory[str] | None]:
  if package_dir is not None and tarball_cache is not None:
    raise ValueError("--package-dir and --tarball-cache cannot be used together")
  if package_dir is not None:
    root = package_dir.resolve()
    temporary = None
  else:
    temporary = tempfile.TemporaryDirectory(prefix="goccia-es-toolkit-")
    if tarball_cache is not None and tarball_cache.is_file():
      data = tarball_cache.read_bytes()
    else:
      data = download_tarball(manifest["upstream"]["tarball"])
    verify_integrity(data, manifest["upstream"]["integrity"])
    if tarball_cache is not None and not tarball_cache.exists():
      tarball_cache.parent.mkdir(parents=True, exist_ok=True)
      tarball_cache.write_bytes(data)
    root = extract_verified_package(data, Path(temporary.name))

  package_metadata = json.loads((root / "package.json").read_text(encoding="utf-8"))
  expected_version = manifest["upstream"]["version"]
  if package_metadata.get("name") != manifest["upstream"]["name"] or package_metadata.get("version") != expected_version:
    raise ValueError(f"expected es-toolkit {expected_version} at {root}")
  return root, temporary


def write_import_map(manifest: dict[str, Any], package_root: Path, directory: Path) -> Path:
  imports = {
    specifier: str((package_root / relative_path).resolve())
    for specifier, relative_path in manifest["modules"].items()
  }
  import_map = directory / "importmap.json"
  import_map.write_text(json.dumps({"imports": imports}, indent=2) + "\n", encoding="utf-8")
  return import_map


def parse_single_marker(stdout: str, marker: str) -> tuple[dict[str, Any] | None, str | None]:
  marker_lines = [line[len(marker):] for line in stdout.splitlines() if line.startswith(marker)]
  if len(marker_lines) != 1:
    return None, f"expected one {marker} marker, found {len(marker_lines)}"
  try:
    return json.loads(marker_lines[0]), None
  except json.JSONDecodeError as error:
    return None, f"invalid {marker} JSON: {error}"


def run_probe(
  goccia: Path,
  probe_path: Path,
  mode: str,
  import_map: Path,
  engine_flags: list[str],
  marker: str,
  timeout_seconds: int,
) -> dict[str, Any]:
  command = [
    str(goccia),
    str(probe_path),
    f"--mode={mode}",
    f"--import-map={import_map}",
    *engine_flags,
  ]
  try:
    process = subprocess.run(
      command,
      text=True,
      encoding="utf-8",
      capture_output=True,
      timeout=timeout_seconds,
      check=False,
    )
  except subprocess.TimeoutExpired as error:
    return {
      "mode": mode,
      "transport": "timeout",
      "exitCode": None,
      "stdout": error.stdout or "",
      "stderr": error.stderr or "",
      "marker": None,
      "markerError": f"probe exceeded {timeout_seconds}s",
    }

  parsed_marker, marker_error = parse_single_marker(process.stdout, marker)
  return {
    "mode": mode,
    "transport": "completed",
    "exitCode": process.returncode,
    "stdout": process.stdout,
    "stderr": process.stderr,
    "marker": parsed_marker,
    "markerError": marker_error,
  }


def is_native_or_harness_failure(result: dict[str, Any]) -> bool:
  if result["transport"] != "completed":
    return True
  exit_code = result["exitCode"]
  if exit_code is None or exit_code < 0 or exit_code > 1:
    return True
  return False


def classify_semantic_probe(probe: dict[str, Any], runs: dict[str, dict[str, Any]]) -> tuple[str, str]:
  interpreted = runs["interpreted"]
  bytecode = runs["bytecode"]
  if is_native_or_harness_failure(interpreted) or is_native_or_harness_failure(bytecode):
    return "harness-failure", "probe timed out, crashed, or exited outside the JavaScript error contract"

  interpreted_marker = interpreted["marker"]
  bytecode_marker = bytecode["marker"]
  if interpreted_marker is None and bytecode_marker is None:
    return "harness-failure", "neither mode produced a result marker"
  if interpreted_marker is None or bytecode_marker is None:
    return "bytecode-divergence", "only one execution mode completed the probe contract"

  if interpreted_marker.get("id") != probe["id"] or bytecode_marker.get("id") != probe["id"]:
    return "harness-failure", "probe marker id does not match the manifest"

  interpreted_status = interpreted_marker.get("status")
  bytecode_status = bytecode_marker.get("status")
  if interpreted_status not in {"pass", "fail"} or bytecode_status not in {"pass", "fail"}:
    return "harness-failure", "probe marker status must be pass or fail"
  if interpreted_status == "pass" and bytecode_status == "pass":
    return "semantic-pass", "both execution modes passed"
  if interpreted_status == bytecode_status == "fail":
    return "semantic-failure", "both execution modes reported the same semantic class of failure"
  return "bytecode-divergence", "execution modes produced different semantic outcomes"


def compact_run(result: dict[str, Any]) -> dict[str, Any]:
  return {
    "transport": result["transport"],
    "exitCode": result["exitCode"],
    "marker": result["marker"],
    "markerError": result["markerError"],
    "stdout": result["stdout"].strip(),
    "stderr": result["stderr"].strip(),
  }


def matches_known_bytecode_divergence(
  probe: dict[str, Any],
  classification: str,
  runs: dict[str, dict[str, Any]],
) -> bool:
  expected = probe.get("knownBytecodeDivergence")
  if classification != "bytecode-divergence" or not expected:
    return False
  interpreted_marker = runs["interpreted"]["marker"] or {}
  bytecode_marker = runs["bytecode"]["marker"] or {}
  return (
    interpreted_marker.get("status") == "pass"
    and bytecode_marker.get("status") == "fail"
    and re.search(expected["errorPattern"], bytecode_marker.get("error", "")) is not None
  )


def build_report(
  manifest: dict[str, Any],
  goccia: Path,
  package_root: Path,
  import_map: Path,
  timeout_seconds: int,
) -> dict[str, Any]:
  semantic_results = []
  summary: dict[str, list[Any]] = {
    "semanticPasses": [],
    "semanticFailures": [],
    "bytecodeDivergences": [],
    "disabledCapabilities": [],
    "hostGlobals": [],
    "harnessFailures": [],
  }

  for probe in manifest["probes"]:
    probe_path = HARNESS_DIRECTORY / probe["file"]
    runs = {
      mode: run_probe(
        goccia,
        probe_path,
        mode,
        import_map,
        manifest["engineFlags"],
        RESULT_MARKER,
        timeout_seconds,
      )
      for mode in MODES
    }
    classification, reason = classify_semantic_probe(probe, runs)
    known_divergence = matches_known_bytecode_divergence(probe, classification, runs)
    result = {
      "id": probe["id"],
      "classification": classification,
      "reason": reason,
      "knownBytecodeDivergence": probe.get("knownBytecodeDivergence"),
      "matchesKnownBytecodeDivergence": known_divergence,
      "modes": {mode: compact_run(runs[mode]) for mode in MODES},
    }
    semantic_results.append(result)
    if classification == "semantic-pass":
      summary["semanticPasses"].append(probe["id"])
    elif classification == "semantic-failure":
      summary["semanticFailures"].append(probe["id"])
    elif classification == "bytecode-divergence":
      summary["bytecodeDivergences"].append({
        "id": probe["id"],
        "known": known_divergence,
        "reason": probe.get("knownBytecodeDivergence", {}).get("reason", reason),
      })
    else:
      summary["harnessFailures"].append({"id": probe["id"], "reason": reason})

  environment_path = HARNESS_DIRECTORY / manifest["environmentProbe"]
  environment_runs = {
    mode: run_probe(
      goccia,
      environment_path,
      mode,
      import_map,
      manifest["engineFlags"],
      ENVIRONMENT_MARKER,
      timeout_seconds,
    )
    for mode in MODES
  }
  for mode, result in environment_runs.items():
    if is_native_or_harness_failure(result) or result["marker"] is None:
      summary["harnessFailures"].append({"id": f"environment:{mode}", "reason": result["markerError"]})

  environment_markers = {
    mode: environment_runs[mode]["marker"]
    for mode in MODES
    if environment_runs[mode]["marker"] is not None
  }
  if len(environment_markers) == len(MODES):
    summary["disabledCapabilities"].append({
      "name": "Function constructor",
      "interpreted": environment_markers["interpreted"]["functionConstructor"],
      "bytecode": environment_markers["bytecode"]["functionConstructor"],
    })
    for name in ("Buffer", "Blob", "process"):
      summary["hostGlobals"].append({
        "name": name,
        "interpreted": environment_markers["interpreted"]["hostGlobals"][name],
        "bytecode": environment_markers["bytecode"]["hostGlobals"][name],
      })

  unexpected_divergences = [item for item in summary["bytecodeDivergences"] if not item["known"]]
  function_capabilities = [
    marker["functionConstructor"]
    for marker in environment_markers.values()
  ]
  function_error_pattern = manifest["expectedEnvironment"]["functionConstructorErrorPattern"]
  default_function_boundary_preserved = len(function_capabilities) == len(MODES) and all(
    capability.get("status") == "disabled"
    and re.search(function_error_pattern, capability.get("error", "")) is not None
    for capability in function_capabilities
  )
  report_ok = not (
    summary["semanticFailures"]
    or unexpected_divergences
    or summary["harnessFailures"]
  ) and default_function_boundary_preserved

  return {
    "schemaVersion": 1,
    "upstream": manifest["upstream"],
    "packageRoot": str(package_root),
    "goccia": str(goccia),
    "engineFlags": manifest["engineFlags"],
    "semanticResults": semantic_results,
    "environment": {mode: compact_run(environment_runs[mode]) for mode in MODES},
    "summary": summary,
    "defaultFunctionBoundaryPreserved": default_function_boundary_preserved,
    "ok": report_ok,
  }


def print_summary(report: dict[str, Any]) -> None:
  summary = report["summary"]
  print(f"es-toolkit {report['upstream']['version']} @ {report['upstream']['commit']}")
  print(f"Semantic probes passed in both modes: {len(summary['semanticPasses'])}")
  print(f"Semantic failures: {len(summary['semanticFailures'])}")
  print(f"Bytecode divergences: {len(summary['bytecodeDivergences'])}")
  for divergence in summary["bytecodeDivergences"]:
    known = "known" if divergence["known"] else "unexpected"
    print(f"  - {divergence['id']} ({known}): {divergence['reason']}")
  print(f"Disabled capabilities: {len(summary['disabledCapabilities'])}")
  print(f"Host globals observed: {len(summary['hostGlobals'])}")
  print(f"Harness failures: {len(summary['harnessFailures'])}")


def main() -> int:
  args = parse_args()
  if args.timeout_seconds < 1:
    raise ValueError("--timeout-seconds must be positive")
  manifest = load_manifest(args.manifest)
  package_root, temporary_package = prepare_package(manifest, args.package_dir, args.tarball_cache)
  try:
    with tempfile.TemporaryDirectory(prefix="goccia-es-toolkit-importmap-") as import_map_directory:
      import_map = write_import_map(manifest, package_root, Path(import_map_directory))
      report = build_report(
        manifest,
        args.goccia.resolve(),
        package_root,
        import_map,
        args.timeout_seconds,
      )
  finally:
    if temporary_package is not None:
      temporary_package.cleanup()

  print_summary(report)
  if args.output:
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
  return 0 if report["ok"] else 1


if __name__ == "__main__":
  try:
    raise SystemExit(main())
  except (OSError, ValueError, urllib.error.URLError) as error:
    print(f"es-toolkit validation harness failure: {error}", file=sys.stderr)
    raise SystemExit(2)
