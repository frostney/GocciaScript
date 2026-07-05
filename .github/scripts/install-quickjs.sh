#!/usr/bin/env bash
set -euo pipefail

version="2026-06-04"
sha256="b376e839b322978313d929fd20663b11ba58b75df5a46c126dd19ea2fa70ad2a"
archive="$RUNNER_TEMP/quickjs-$version.tar.xz"
prefix="$RUNNER_TEMP/quickjs"

curl -fsSLo "$archive" "https://bellard.org/quickjs/quickjs-$version.tar.xz"
echo "$sha256  $archive" | shasum -a 256 --check
tar -C "$RUNNER_TEMP" -xf "$archive"
make -C "$RUNNER_TEMP/quickjs-$version" install PREFIX="$prefix" CONFIG_M32=
echo "$prefix/bin" >> "$GITHUB_PATH"
"$prefix/bin/qjs" --eval 'print("QuickJS " + (1 + 2))'
