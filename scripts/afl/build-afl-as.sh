#!/usr/bin/env bash
# Builds AFL++'s afl-as, which scripts/afl/as hands FPC's assembly to, into
# build/afl/afl-as (or the directory given as the only argument).
#
#   scripts/afl/build-afl-as.sh [<output directory>]
#
# AFL++ removed afl-as after v4.21c (AFLplusplus/AFLplusplus@7b24f4a329,
# released in 4.30c), so newer AFL++ packages, Ubuntu 26.04's among them,
# have none. This takes it from the v4.21c release, checked against its
# SHA-256, and builds only afl-as, which needs curl with access to GitHub, a
# C compiler and make. The afl-fuzz and afl-showmap of a current AFL++ drive
# the harness it instruments: its fork server speaks the original handshake,
# which AFL++ still accepts ("Old fork server model is used by the target,
# this still works though").
set -euo pipefail

version=v4.21c
sha256=11f7c77d37cff6e7f65ac7cc55bab7901e0c6208e845a38764394d04ed567b30
url="https://github.com/AFLplusplus/AFLplusplus/archive/refs/tags/${version}.tar.gz"

if [ $# -gt 1 ]; then
  echo "usage: scripts/afl/build-afl-as.sh [<output directory>]" >&2
  exit 2
fi
root=$(CDPATH= cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd)
out="${1:-$root/build/afl}"

work=$(mktemp -d "${TMPDIR:-/tmp}/goccia-afl-as-XXXXXX")
trap 'rm -rf "$work"' EXIT

curl -fsSL --retry 3 -o "$work/aflplusplus.tar.gz" "$url"
# The record names the archive relative to the work directory, so no part of
# TMPDIR goes into sha256sum's line-based input.
(cd "$work" && echo "$sha256  aflplusplus.tar.gz" | sha256sum -c --quiet -)
mkdir "$work/src"
tar -xzf "$work/aflplusplus.tar.gz" -C "$work/src" --strip-components=1
make -C "$work/src" -s afl-as > "$work/make.log" 2>&1 || {
  cat "$work/make.log" >&2
  exit 1
}

install -D -T -m 755 -- "$work/src/afl-as" "$out/afl-as"
echo "afl-as from AFL++ ${version} is at $out/afl-as"
