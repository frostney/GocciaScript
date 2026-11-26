#!/bin/sh
# scripts/engine/gate.sh NAME [BASE]: the engine gate. Runs test262 (contract 4: every category, both modes where the
# test needs them) with build/engine/goccia into build/engine/gate/NAME.json, compares it with build/engine/gate/BASE.json when
# given, and checks the Web Tooling digests against scripts/engine/digests.txt.
#   GOCCIA_TEST262  a test262 checkout (pinned 91c70759)
#   GOCCIA_WT_DIR   the Web Tooling bundles that print a DIGEST line (web-tooling-*.js)
#   GOCCIA_JOBS     parallel test262 workers (default 16)
set -u
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
cd "$ROOT"
N=$1; B=${2:-}
OUT=build/engine/gate
mkdir -p "$OUT"
BIN=$OUT/goccia-$N
cp build/engine/goccia "$BIN"
python3 scripts/engine/run262.py "$GOCCIA_TEST262" "$BIN" "$OUT/$N.json" "${GOCCIA_JOBS:-16}" > "$OUT/$N-summary.txt" 2> "$OUT/$N.err"
tail -1 "$OUT/$N-summary.txt"
[ -n "$B" ] && python3 scripts/engine/diff_c4.py "$OUT/$B.json" "$OUT/$N.json" 60 > "$OUT/$N-diff.txt" && sed -n '1,12p;/^newly failing/,/^newly passing/p' "$OUT/$N-diff.txt" | head -40
for f in "$GOCCIA_WT_DIR"/web-tooling-*.js; do
  n=$(basename "$f" .js)
  ( d=$(timeout 900 "$BIN" "$f" 2>&1 | grep -a 'DIGEST' | head -1); echo "$n | $d" ) &
done > "$OUT/$N-digests.raw" 2>/dev/null
wait
python3 - "$OUT/$N-digests.raw" scripts/engine/digests.txt <<'PY'
import sys
new = {l.split(' | ')[0]: l.split(' | ', 1)[1].strip() for l in open(sys.argv[1]) if ' | ' in l}
ref = {l.split(' | ')[0]: l.split(' | ')[1].strip() for l in open(sys.argv[2]) if ' | ' in l}
bad = [n for n in ref if ref[n] != new.get(n)]
print('digests', len(new), 'mismatch:', bad)
for n in bad: print(n, ref[n], '|', new.get(n))
PY
