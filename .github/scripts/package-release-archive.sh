#!/usr/bin/env bash

# Packages one target's downloaded build artifact into a release archive, the
# way the tagged release (ci.yml) and the nightly prerelease (nightly.yml)
# publish it: the shipped binaries plus tests/, benchmarks/ and examples/
# under a single gocciascript-<label>-<platform>/ directory. The archive is
# then checked with check-release-archive.sh, so a binary that is missing or
# not executable fails the packaging instead of reaching a release.
#
# This script owns the published asset names, so the release and the nightly
# cannot drift apart: .tar.gz for Linux, .zip for macOS and Windows. Homebrew,
# the website installers and packaging/apt depend on them.
#
#   package-release-archive.sh --targets
#       Prints the supported targets, one per line.
#   package-release-archive.sh <target> <artifact-dir> <label>
#       Writes gocciascript-<label>-<platform>.<ext> to the current directory.
#
# Run from the repository root.

set -euo pipefail

targets=(aarch64-darwin x86_64-darwin x86_64-linux aarch64-linux x86_64-win64 i386-win32)

usage() {
  echo "Usage: $0 --targets" >&2
  echo "       $0 <target> <artifact-dir> <label>" >&2
  exit 1
}

if [ "$#" -eq 1 ] && [ "$1" = "--targets" ]; then
  printf '%s\n' "${targets[@]}"
  exit 0
fi

if [ "$#" -ne 3 ]; then
  usage
fi

target="$1"
artifact_dir="$2"
label="$3"
script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

case "$target" in
  aarch64-darwin) platform="macos-arm64"; format="zip" ;;
  x86_64-darwin)  platform="macos-x64"; format="zip" ;;
  x86_64-linux)   platform="linux-x64"; format="tar.gz" ;;
  aarch64-linux)  platform="linux-arm64"; format="tar.gz" ;;
  x86_64-win64)   platform="windows-x64"; format="zip" ;;
  i386-win32)     platform="windows-x86"; format="zip" ;;
  *)
    echo "::error::Unknown release target: $target"
    exit 1
    ;;
esac

if [ -z "$label" ]; then
  usage
fi

archive_name="gocciascript-${label}-${platform}"
pkg_dir="$archive_name"
archive="${archive_name}.${format}"

if [ -e "$pkg_dir" ]; then
  echo "::error::Staging directory $pkg_dir already exists"
  exit 1
fi

# Stages only the binaries derived from source/app/*.dpr and restores their
# executable bit, which upload-artifact/download-artifact do not keep.
bash "$script_dir/stage-build-artifacts.sh" "$artifact_dir" "$pkg_dir"

cp -r tests/ "$pkg_dir/tests/"
cp -r benchmarks/ "$pkg_dir/benchmarks/"
cp -r examples/ "$pkg_dir/examples/"

rm -f "$archive"
if [ "$format" = "zip" ]; then
  zip -q -r "$archive" "$pkg_dir/"
else
  tar -czf "$archive" "$pkg_dir/"
fi

rm -rf "$pkg_dir"

bash "$script_dir/check-release-archive.sh" "$archive"
