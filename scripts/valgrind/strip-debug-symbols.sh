#!/usr/bin/env bash
# Removes the debugging symbols (nm type N) from FPC executables before they
# run under Valgrind, keeping the DWARF sections their line numbers come from:
#
#   bash scripts/valgrind/strip-debug-symbols.sh build/GCStressMemcheck ...
#
# FPC 3.3.1 writes a symbol for each DWARF label (DBG_..., DBGREF_...,
# .Ldebug_info0 and so on). Its value is an offset into a debug section, which
# Valgrind takes for a code address when it names a function, so a frame
# inside, say, IndexByte is reported under a DBG_ name that no suppression
# can follow.
set -euo pipefail
for bin in "$@"; do
  list=$(mktemp "${TMPDIR:-/tmp}/goccia-debug-symbols-XXXXXX")
  nm "$bin" | awk '$2 == "N" { print $3 }' > "$list"
  objcopy --strip-symbols="$list" "$bin"
  rm -f "$list"
done
