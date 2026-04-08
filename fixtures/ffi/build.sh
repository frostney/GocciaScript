#!/bin/sh
# Build the FFI test fixture library for the current platform.
# Usage: ./fixtures/ffi/build.sh

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SRC="$SCRIPT_DIR/fixture.c"

case "$(uname -s)" in
  Darwin*)
    cc -shared -o "$SCRIPT_DIR/libfixture.dylib" "$SRC"
    echo "Built $SCRIPT_DIR/libfixture.dylib"
    ;;
  Linux*)
    cc -shared -fPIC -o "$SCRIPT_DIR/libfixture.so" "$SRC"
    echo "Built $SCRIPT_DIR/libfixture.so"
    ;;
  MINGW*|MSYS*|CYGWIN*)
    gcc -shared -o "$SCRIPT_DIR/fixture.dll" "$SRC"
    echo "Built $SCRIPT_DIR/fixture.dll"
    ;;
  *)
    echo "Unsupported platform: $(uname -s)" >&2
    exit 1
    ;;
esac
