#!/usr/bin/env bash

# Packages one target's downloaded build artifact into a release archive, the
# way the tagged release (ci.yml) and the nightly prerelease (nightly.yml)
# publish it: the shipped binaries plus tests/, benchmarks/ and examples/
# under a single <archive-name>/ directory, archived as
# <archive-name>.<format>. The archive is then checked with
# check-release-archive.sh, so a binary that is missing or not executable
# fails the packaging instead of reaching a release.
#
# Run from the repository root. The caller picks the format per target; the
# published asset names are .tar.gz for Linux and .zip for macOS and Windows.

set -euo pipefail

usage() {
  echo "Usage: $0 <artifact-dir> <archive-name> <tar.gz|zip>" >&2
  exit 1
}

if [ "$#" -ne 3 ]; then
  usage
fi

artifact_dir="$1"
archive_name="$2"
format="$3"
pkg_dir="$archive_name"
script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

case "$format" in
  tar.gz|zip) ;;
  *) usage ;;
esac

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

archive="${archive_name}.${format}"
rm -f "$archive"
if [ "$format" = "zip" ]; then
  zip -q -r "$archive" "$pkg_dir/"
else
  tar -czf "$archive" "$pkg_dir/"
fi

rm -rf "$pkg_dir"

bash "$script_dir/check-release-archive.sh" "$archive"
