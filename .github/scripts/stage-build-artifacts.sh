#!/usr/bin/env bash

set -euo pipefail

usage() {
  echo "Usage: $0 <build-dir> <artifact-dir> [--include-tests] [--strip]" >&2
  exit 1
}

if [ "$#" -lt 2 ] || [ "$#" -gt 4 ]; then
  usage
fi

build_dir="$1"
artifact_dir="$2"
shift 2

include_tests=""
do_strip=""
for arg in "$@"; do
  case "$arg" in
    --include-tests) include_tests=1 ;;
    --strip)         do_strip=1 ;;
    *)               usage ;;
  esac
done

mkdir -p "$artifact_dir"

shopt -s nullglob

entrypoints=(./source/app/*.dpr)
if [ "${#entrypoints[@]}" -eq 0 ]; then
  echo "::error::No .dpr entrypoints found in source/app/"
  exit 1
fi

for src in "${entrypoints[@]}"; do
  name="$(basename "${src%.dpr}")"
  copied_binary=0

  for candidate in "$build_dir/$name" "$build_dir/$name.exe"; do
    if [ -f "$candidate" ]; then
      cp "$candidate" "$artifact_dir/"
      copied_binary=1
    fi
  done

  if [ "$copied_binary" -eq 0 ]; then
    echo "::error::Missing compiled binary for $src in $build_dir"
    exit 1
  fi
done

if [ -n "$include_tests" ]; then
  copied_test=0

  for test_bin in "$build_dir"/*.Test "$build_dir"/*.Test.exe; do
    if [ -f "$test_bin" ]; then
      cp "$test_bin" "$artifact_dir/"
      copied_test=1
    fi
  done

  if [ "$copied_test" -eq 0 ]; then
    echo "::error::No Pascal unit test binaries found in $build_dir"
    exit 1
  fi
fi

if [ -n "$do_strip" ]; then
  for bin in "$artifact_dir"/*; do
    if [ -f "$bin" ]; then
      # Prefer llvm-strip (handles Mach-O, ELF, and PE/COFF) over platform strip
      llvm-strip "$bin" 2>/dev/null || strip "$bin" 2>/dev/null || true
    fi
  done
fi

echo "Staged artifacts:"
ls -la "$artifact_dir"
