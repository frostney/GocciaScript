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
# against its SHA-256; building them needs curl with access to GitHub, a C
# compiler and make.
#
# All three are built with a 1 MiB coverage map in place of AFL++'s 64 KiB
# (MAP_SIZE_POW2 16), which the harness's edges fill within minutes, so that
# edges stop colliding and hiding new coverage. afl-as draws each
# trampoline's random edge ID from that range, and a target with the original
# fork server cannot tell afl-fuzz or afl-showmap its map size: they take it
# from their own build (see docs/contributing/tooling.md). So afl-fuzz and
# afl-showmap get the same MAP_SIZE, and DEFAULT_SHMEM_SIZE, their starting
# map, is set to it in place of 8 MiB, so each map is exactly the harness's.
set -euo pipefail

map_size_pow2=20

if [ $# -gt 1 ]; then
  echo "usage: scripts/afl/build-afl.sh [<output directory>]" >&2
  exit 2
fi
root=$(CDPATH= cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd)
out="${1:-$root/build/afl}"

work=$(mktemp -d "${TMPDIR:-/tmp}/goccia-afl-XXXXXX")
trap 'rm -rf "$work"' EXIT

# define <directory> <name> <AFL++'s value> <ours>: replaces one #define in
# <directory>'s include/config.h, and stops if AFL++'s line is not there.
define() {
  local config=$work/$1/include/config.h name=$2 from=$3 to=$4
  grep -qxF "#define $name $from" "$config" || {
    echo "scripts/afl/build-afl.sh: no '#define $name $from' in $1's include/config.h" >&2
    exit 1
  }
  awk -v line="#define $name $from" -v new="#define $name $to" \
    '$0 == line { $0 = new } { print }' "$config" > "$config.new"
  mv "$config.new" "$config"
}

# fetch <release> <sha256> <directory>: downloads and checks the release and
# unpacks it into <directory>.
fetch() {
  local release=$1 sha256=$2 name=$3 dir=$work/$3
  curl -fsSL --retry 3 -o "$dir.tar.gz" \
    "https://github.com/AFLplusplus/AFLplusplus/archive/refs/tags/${release}.tar.gz"
  # The record names the archive relative to the work directory, so no part
  # of TMPDIR goes into sha256sum's line-based input.
  (cd "$work" && echo "$sha256  $name.tar.gz" | sha256sum -c --quiet -)
  mkdir "$dir"
  tar -xzf "$dir.tar.gz" -C "$dir" --strip-components=1
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

cpus=$(nproc)

fetch v4.21c 11f7c77d37cff6e7f65ac7cc55bab7901e0c6208e845a38764394d04ed567b30 as
define as MAP_SIZE_POW2 16 "$map_size_pow2"
build as afl-as

fetch v5.03c 07f089e8591209862898c770a569d8e2b74b459fe967db323fc6a3924dcc82b5 fuzz
define fuzz MAP_SIZE_POW2 16 "$map_size_pow2"
define fuzz DEFAULT_SHMEM_SIZE '(8 * 1024 * 1024)' MAP_SIZE
build fuzz -j"$cpus" NO_PYTHON=1 afl-fuzz afl-showmap

install -D -T -m 755 -- "$work/as/afl-as" "$out/afl-as"
install -D -T -m 755 -- "$work/fuzz/afl-fuzz" "$out/afl-fuzz"
install -D -T -m 755 -- "$work/fuzz/afl-showmap" "$out/afl-showmap"
echo "afl-as (AFL++ v4.21c), afl-fuzz and afl-showmap (v5.03c) are in $out"
