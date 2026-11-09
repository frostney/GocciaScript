#!/bin/bash
# scripts/g2/gcstress.sh [KB]: every Web Tooling (digest) and JetStream bundle under a small nursery (default
# 256 KB) with --gc-verify; a GC rooting bug shows here long before the default nursery does.
#   G2_WT_DIR  the Web Tooling digest bundles; G2_JS_DIR  the JetStream bundles (jetstream-*.js)
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
cd "$ROOT"
G2=build/g2/g2; KB=${1:-256}; ref=scripts/g2/digests.txt
run() {
  f=$1; n=$(basename $f .js)
  out=$(G2_NURSERY_KB=$KB timeout 1800 $G2 --gc-verify $f 2>&1)
  case $n in
    web-tooling-*) d=$(echo "$out" | grep -a DIGEST | head -1); r=$(grep "^$n |" $ref | cut -d'|' -f2 | xargs); [ "$d" = "$r" ] && echo "OK $n" || echo "BAD $n: $(echo "$out" | grep -av '^\s*$' | tail -1 | cut -c1-120)";;
    *) if echo "$out" | grep -q '"verificationPassed":true' || { echo "$out" | grep -q '"verificationKind":"execution' && ! echo "$out" | grep -q Uncaught; }; then echo "OK $n"; else echo "BAD $n: $(echo "$out" | grep -av '^\s*$' | tail -1 | cut -c1-120)"; fi;;
  esac
}
export -f run; export G2 KB ref
ls "$G2_WT_DIR"/web-tooling-*.js "$G2_JS_DIR"/jetstream-*.js | xargs -P 16 -I{} bash -c 'run {}' | sort > build/g2/gcstress.txt
grep -c "^OK" build/g2/gcstress.txt | sed "s/^/ok: /"; grep "^BAD" build/g2/gcstress.txt; true
