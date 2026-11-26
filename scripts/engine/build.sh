#!/bin/sh
# scripts/engine/build.sh: builds the G2 engine into build/engine/goccia with `./build.pas --prod engine`, under fpc-toolchain's
# FPC 3.3.1+9 (3.3.1-b9), the build CI uses. GOCCIA_FPC_ENV overrides the command that selects the toolchain.
set -u
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
cd "$ROOT"
if [ -n "${GOCCIA_FPC_ENV:-}" ]; then eval "$GOCCIA_FPC_ENV"; else eval "$("$HOME/.local/share/fpc-toolchains/bin/fpc-toolchain" env 3.3.1+9)"; fi
mkdir -p build/engine
./build.pas --prod engine > build/engine/build.log 2>&1; rc=$?
tail -5 build/engine/build.log
exit $rc
