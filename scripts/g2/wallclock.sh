#!/bin/bash
# scripts/g2/wallclock.sh G2 [G2B]: best-of-3 wall clock per JetStream and Web Tooling bundle, one at a time,
# against QuickJS. Prints QuickJS time / G2 time per bundle (above 1: G2 is faster) and the geomeans; with G2B,
# also G2B / G2. Run it on a quiet machine.
#   G2_QJS  the qjs binary; G2_WT_DIR, G2_JS_DIR  the bundle directories
A=$1; B=${2:-}
best() { local b=999999; for i in 1 2 3; do s=$(date +%s%N); "$@" > /dev/null 2>&1; e=$(date +%s%N); t=$(( (e - s) / 1000000 )); [ $t -lt $b ] && b=$t; done; echo $b; }
for f in "$G2_JS_DIR"/jetstream-*.js "$G2_WT_DIR"/web-tooling-*.js; do
  n=$(basename "$f" .js); q=$(best "$G2_QJS" "$f"); a=$(best "$A" "$f")
  if [ -n "$B" ]; then b=$(best "$B" "$f"); else b=$a; fi
  echo "$n $q $a $b"
done | awk '{ printf "%-38s qjs %6d g2 %6d g2b %6d  qjs/g2 %.3f  g2b/g2 %.3f\n", $1, $2, $3, $4, $2 / $3, $4 / $3;
  k = ($1 ~ /^jetstream/) ? "jetstream" : "web-tooling"; s[k] += log($2 / $3); sb[k] += log($4 / $3); c[k]++;
  if ($1 != "web-tooling-coffeescript" && k == "web-tooling") { x += log($2 / $3); xc++ } }
  END { for (k in c) printf "%s geomean qjs/g2 %.3f  g2b/g2 %.3f (n=%d)\n", k, exp(s[k] / c[k]), exp(sb[k] / c[k]), c[k];
        if (xc) printf "web-tooling excluding coffeescript qjs/g2 %.3f\n", exp(x / xc) }'
