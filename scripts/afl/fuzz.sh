#!/usr/bin/env bash
# Runs one bounded afl-fuzz campaign over the instrumented harness, as both
# AFL jobs of .github/workflows/fuzz.yml do:
#
#   scripts/afl/fuzz.sh <seconds> <seed directory> <output directory> \
#     -- <harness> [<argument>...]
#
# The harness arguments name the input as @@, as afl-fuzz expects. It exits 0
# only when afl-fuzz fuzzed for <seconds> and stopped by itself; findings are
# in <output directory>/default/crashes and hangs either way.
#
# - AFL_SKIP_CPUFREQ and AFL_NO_AFFINITY are the usual accommodations for
#   shared runners.
# - -t 5000: the harness's own budget is 1000 ms and a harness timeout is a
#   normal exit-0 outcome, so AFL's exec threshold sits well above it;
#   otherwise process overhead around a legitimate slow input lands it in
#   hangs/ and fails the job falsely.
# - -V: afl-fuzz stops after <seconds> of fuzzing, not counting the seeds'
#   calibration, and writes its final stats. It does not reliably exit on a
#   SIGTERM: run 31771115536 sat 43 more minutes on one until the job's
#   60-minute cap cancelled it, and v5.03c ignored timeout's every time (run
#   45623922253). timeout -k 60 stays as a backstop 20 minutes past
#   <seconds>, for the calibration (about 3 minutes for the script corpus on
#   a runner). afl-fuzz still running then has stalled, which fails the run.
#   Findings are written to crashes/ and hangs/ as they are discovered, so a
#   kill loses no reproducers, only the final stats.
set -euo pipefail

usage() {
  echo "usage: scripts/afl/fuzz.sh <seconds> <seed directory> <output directory> -- <harness> [<argument>...]" >&2
  exit 2
}
[ $# -ge 5 ] && [ "$4" = -- ] || usage
seconds=$1
seeds=$2
out=$3
shift 4

# A whole number of seconds, read in base 10: the workflow's dispatch input
# arrives as text, and shell arithmetic would read 0900 as octal.
if ! [[ $seconds =~ ^[0-9]+$ ]] || ((10#$seconds == 0)); then
  echo "::error::fuzz seconds must be a positive whole number, not '$seconds'" >&2
  exit 2
fi
seconds=$((10#$seconds))
deadline=$((seconds + 1200))

mkdir -p "$out"
status=0
started=$SECONDS
AFL_SKIP_CPUFREQ=1 AFL_NO_AFFINITY=1 AFL_BENCH_UNTIL_CRASH=1 \
  timeout -k 60 "$deadline" afl-fuzz -V "$seconds" -t 5000 \
  -i "$seeds" -o "$out" -- "$@" || status=$?
elapsed=$((SECONDS - started))
echo "afl-fuzz exit status: ${status} after ${elapsed}s"

# 0 is afl-fuzz stopping at its -V limit, or at the first crash under
# AFL_BENCH_UNTIL_CRASH, which the workflow's crash report then fails on.
# 124 and 137 at the deadline are the backstop; 137 before it is an outside
# kill, such as the OOM killer. Anything else is afl-fuzz not getting off the
# ground (bad arguments, a missing harness, 127 for a missing afl-fuzz).
case $status in
  0) ;;
  124 | 137)
    if [ "$elapsed" -ge "$deadline" ]; then
      echo "::error::afl-fuzz was still running ${deadline}s in, past its ${seconds}s limit, and the backstop stopped it (exit ${status})"
    else
      echo "::error::afl-fuzz was killed after ${elapsed}s, before its ${deadline}s backstop (exit ${status}; the OOM killer?)"
    fi
    exit 1
    ;;
  *)
    echo "::error::afl-fuzz did not run to its limit (exit ${status})"
    exit "$status"
    ;;
esac
