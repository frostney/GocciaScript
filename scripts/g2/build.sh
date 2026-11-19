#!/bin/sh
# scripts/g2/build.sh: builds the G2 engine into build/g2/g2 with `./build.pas --prod g2`, under fpc-toolchain's
# FPC 3.3.1+4 (3.3.1-b4) or later. G2_FPC_ENV overrides the command that selects the toolchain.
set -u
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
cd "$ROOT"
if [ -n "${G2_FPC_ENV:-}" ]; then eval "$G2_FPC_ENV"; else eval "$("$HOME/.local/share/fpc-toolchains/bin/fpc-toolchain" env 3.3.1+4)"; fi
mkdir -p build/g2
./build.pas --prod g2 > build/g2/build.log 2>&1; rc=$?
tail -5 build/g2/build.log
exit $rc
