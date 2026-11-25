#!/usr/bin/env bash
# Builds AFL++'s afl-as, which scripts/afl/as hands FPC's assembly to, into
# build/afl/afl-as (or the directory given as the only argument).
#
#   scripts/afl/build-afl-as.sh [<output directory>]
#
# AFL++ removed afl-as after v4.21c (AFLplusplus/AFLplusplus@7b24f4a329,
# released in 4.30c), so newer AFL++ packages, Ubuntu 26.04's among them,
# have none. This takes it from the v4.21c release, checked against its
# SHA-256, and builds only afl-as, which needs a C compiler and make. The
# afl-fuzz and afl-showmap of a current AFL++ drive the harness it
# instruments: its fork server speaks the original handshake, which AFL++
# still accepts ("Old fork server model is used by the target, this still
# works though").
#
# afl-as compiles its map size into every trampoline, and AFL++'s default,
# 64 KiB (MAP_SIZE_POW2 16), is mostly full after a few minutes of fuzzing
# the harness, so edges collide and new coverage goes unseen. This builds it
# with a 1 MiB map instead. afl-fuzz and afl-showmap must then use a map at
# least that large: the size is written to map-size next to afl-as, for
# AFL_MAP_SIZE. Without it, Ubuntu 26.04's afl-showmap sizes its map at
# 64 KiB and the harness dies of SIGSEGV writing past it.
set -euo pipefail

version=v4.21c
sha256=11f7c77d37cff6e7f65ac7cc55bab7901e0c6208e845a38764394d04ed567b30
url="https://github.com/AFLplusplus/AFLplusplus/archive/refs/tags/${version}.tar.gz"
map_size_pow2=20

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
out="${1:-$root/build/afl}"

work=$(mktemp -d "${TMPDIR:-/tmp}/goccia-afl-as-XXXXXX")
trap 'rm -rf "$work"' EXIT

curl -fsSL --retry 3 -o "$work/aflplusplus.tar.gz" "$url"
echo "$sha256  $work/aflplusplus.tar.gz" | sha256sum -c --quiet -
mkdir "$work/src"
tar -xzf "$work/aflplusplus.tar.gz" -C "$work/src" --strip-components=1
config="$work/src/include/config.h"
sed -i.orig "s/^#define MAP_SIZE_POW2 16\$/#define MAP_SIZE_POW2 ${map_size_pow2}/" "$config"
grep -qx "#define MAP_SIZE_POW2 ${map_size_pow2}" "$config" || {
  echo "scripts/afl/build-afl-as.sh: could not set MAP_SIZE_POW2 in include/config.h" >&2
  exit 1
}
make -C "$work/src" -s afl-as > "$work/make.log" 2>&1 || {
  cat "$work/make.log" >&2
  exit 1
}

mkdir -p "$out"
install -m 755 "$work/src/afl-as" "$out/afl-as"
echo $((1 << map_size_pow2)) > "$out/map-size"
echo "afl-as from AFL++ ${version} is at $out/afl-as; run afl-fuzz and afl-showmap with AFL_MAP_SIZE=$(cat "$out/map-size")"
