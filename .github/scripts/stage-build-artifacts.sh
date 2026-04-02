#!/usr/bin/env bash

set -euo pipefail

if [ "$#" -lt 2 ] || [ "$#" -gt 3 ]; then
  echo "Usage: $0 <build-dir> <artifact-dir> [--include-tests]" >&2
  exit 1
fi

build_dir="$1"
artifact_dir="$2"
include_tests="${3:-}"

mkdir -p "$artifact_dir"

shopt -s nullglob

entrypoints=(./*.dpr)
if [ "${#entrypoints[@]}" -eq 0 ]; then
  echo "::error::No top-level .dpr entrypoints found"
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

if [ "$include_tests" = "--include-tests" ]; then
  copied_test=0

  for test_bin in "$build_dir"/Goccia.*.Test "$build_dir"/Goccia.*.Test.exe; do
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

echo "Staged artifacts:"
ls -la "$artifact_dir"
