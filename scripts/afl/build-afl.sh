#!/usr/bin/env bash
# Builds the AFL++ tools the instrumented fuzz harness needs into build/afl
# (or the directory given as the only argument): afl-as, which
# scripts/afl/as hands FPC's assembly to, and the afl-fuzz and afl-showmap
# that drive the harness it instruments.
#
#   scripts/afl/build-afl.sh [<output directory>]
#
# AFL++ removed afl-as after v4.21c (AFLplusplus/AFLplusplus@7b24f4a329,
# released in 4.30c), so afl-as comes from the v4.21c release, and afl-fuzz
# and afl-showmap from a current one. Their fork server speaks the original
# handshake afl-as adds, which AFL++ still accepts ("Old fork server model is
# used by the target, this still works though"). Each release is checked
# against its SHA-256, and building these needs a C compiler and make.
#
# All three are built with a 1 MiB coverage map in place of AFL++'s 64 KiB
# (MAP_SIZE_POW2 16), which the harness's edges fill within minutes, so that
# edges stop colliding and hiding new coverage. afl-as writes the map size
# into every trampoline, and AFL++ sizes the map of a target with the
# original fork server at its own build's map size, whatever AFL_MAP_SIZE
# says. A packaged afl-fuzz or afl-showmap therefore gives such a harness a
# 64 KiB map, and the harness dies of SIGSEGV writing past it.
set -euo pipefail

map_size_pow2=20

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
out="${1:-$root/build/afl}"

work=$(mktemp -d "${TMPDIR:-/tmp}/goccia-afl-XXXXXX")
trap 'rm -rf "$work"' EXIT

# fetch <release> <sha256> <directory>: downloads and checks the release,
# unpacks it into <directory> and sets its map size.
fetch() {
  local release=$1 sha256=$2 dir=$work/$3
  curl -fsSL --retry 3 -o "$dir.tar.gz" \
    "https://github.com/AFLplusplus/AFLplusplus/archive/refs/tags/${release}.tar.gz"
  echo "$sha256  $dir.tar.gz" | sha256sum -c --quiet -
  mkdir "$dir"
  tar -xzf "$dir.tar.gz" -C "$dir" --strip-components=1
  sed -i.orig "s/^#define MAP_SIZE_POW2 16\$/#define MAP_SIZE_POW2 ${map_size_pow2}/" \
    "$dir/include/config.h"
  grep -qx "#define MAP_SIZE_POW2 ${map_size_pow2}" "$dir/include/config.h" || {
    echo "scripts/afl/build-afl.sh: could not set MAP_SIZE_POW2 in ${release}'s include/config.h" >&2
    exit 1
  }
}

# build <directory> <make arguments...>
build() {
  local dir=$work/$1
  shift
  make -C "$dir" -s "$@" > "$dir.log" 2>&1 || {
    cat "$dir.log" >&2
    exit 1
  }
}

fetch v4.21c 11f7c77d37cff6e7f65ac7cc55bab7901e0c6208e845a38764394d04ed567b30 as
build as afl-as
fetch v5.03c 07f089e8591209862898c770a569d8e2b74b459fe967db323fc6a3924dcc82b5 fuzz
build fuzz -j"$(nproc)" NO_PYTHON=1 afl-fuzz afl-showmap

mkdir -p "$out"
install -m 755 "$work/as/afl-as" "$work/fuzz/afl-fuzz" "$work/fuzz/afl-showmap" "$out/"
echo "afl-as (AFL++ v4.21c), afl-fuzz and afl-showmap (v5.03c) are in $out"
