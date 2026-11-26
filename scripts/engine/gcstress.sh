#!/bin/bash
# scripts/engine/gcstress.sh [KB]: every Web Tooling (digest) and JetStream bundle (and AWFY bundle, when given,
# without --gc-verify) under a small nursery (default 256 KB) with --gc-verify; a GC rooting bug shows here long
# before the default nursery does. Then each scripts/engine/regress/*.js under 16, 64 and 128 KB nurseries (each prints ok last;
# GOCCIA_HOST gives them GocciaHost.gc()), and every test file under tests/ (scripts/engine/gcstress-tests.py).
#   GOCCIA_WT_DIR  the Web Tooling digest bundles; GOCCIA_JS_DIR  the JetStream bundles (jetstream-*.js);
#   GOCCIA_AWFY_DIR  the AWFY bundles (awfy-*.js, optional); GOCCIA_ENGINE_BIN  the binary (default build/engine/goccia)
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
cd "$ROOT"
G2=${GOCCIA_ENGINE_BIN:-build/engine/goccia}; KB=${1:-256}; ref=scripts/engine/digests.txt
run() {
  f=$1; n=$(basename $f .js)
  # AWFY without --gc-verify: verifying after every collection walks the whole heap, and Havlak's takes hours
  case $n in awfy-*) v=;; *) v=--gc-verify;; esac
  out=$(GOCCIA_NURSERY_KB=$KB timeout 3600 $G2 $v $f 2>&1)
  case $n in
    web-tooling-*) d=$(echo "$out" | grep -a DIGEST | head -1); r=$(grep "^$n |" $ref | cut -d'|' -f2 | xargs); [ "$d" = "$r" ] && echo "OK $n" || echo "BAD $n: $(echo "$out" | grep -av '^\s*$' | tail -1 | cut -c1-120)";;
    *) if echo "$out" | grep -q '"verificationPassed":true' || { echo "$out" | grep -q '"verificationKind":"execution' && ! echo "$out" | grep -q Uncaught; }; then echo "OK $n"; else echo "BAD $n: $(echo "$out" | grep -av '^\s*$' | tail -1 | cut -c1-120)"; fi;;
  esac
}
regress() {
  f=$1; n=$(basename $f .js)
  # a first line "// goccia-flags: ..." adds options (--gc-stress: a collection at every allocation)
  flags=$(sed -n '1s|^// goccia-flags: ||p' $f)
  for kb in 16 64 128; do
    # 1800 s: overflow-add takes ~920 s under --gc-verify at 16 KB on a loaded machine
    r=$(GOCCIA_HOST=1 GOCCIA_NURSERY_KB=$kb timeout 1800 $G2 --gc-verify $flags $f 2>&1; echo "__rc=$?")
    rc=$(echo "$r" | sed -n 's/^__rc=//p'); r=$(echo "$r" | grep -av '^__rc=' | grep -av '^\s*$' | tail -1)
    # a timeout is not a failure, but it hides the sizes after it: say so, so it is rerun rather than written off
    [ "$rc" = 124 ] && { echo "TIMEOUT regress-$n ($kb KB): rerun it at 16, 64 and 128 KB"; return; }
    [ "$r" = "ok" ] || { echo "BAD regress-$n ($kb KB): $(echo "$r" | cut -c1-120)"; return; }
  done
  echo "OK regress-$n"
}
export -f run regress; export G2 KB ref
{ ls "$GOCCIA_WT_DIR"/web-tooling-*.js "$GOCCIA_JS_DIR"/jetstream-*.js
  [ -n "${GOCCIA_AWFY_DIR:-}" ] && ls "$GOCCIA_AWFY_DIR"/awfy-*.js; } | xargs -P 16 -I{} bash -c 'run {}' | sort > build/engine/gcstress.txt
ls scripts/engine/regress/*.js | xargs -P 8 -I{} bash -c 'regress {}' | sort >> build/engine/gcstress.txt
# GocciaScript's own tests through goccia --test at a 16 KB nursery (scripts/engine/gcstress-tests.py): one OK line
python3 scripts/engine/gcstress-tests.py "$G2" 12 tests >> build/engine/gcstress.txt
grep -c "^OK" build/engine/gcstress.txt | sed "s/^/ok: /"; grep "^BAD\|^TIMEOUT" build/engine/gcstress.txt; true
