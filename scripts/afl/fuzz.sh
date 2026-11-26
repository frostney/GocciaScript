#!/usr/bin/env bash
# Runs one bounded afl-fuzz campaign over the instrumented harness, as both
# AFL jobs of .github/workflows/fuzz.yml do:
#
#   scripts/afl/fuzz.sh <seconds> <seed directory> <output directory> \
#     -- <harness> [<argument>...]
#
# <seconds> is a whole number from 1 to 3600. The harness arguments name the
# input as @@, as afl-fuzz expects. It exits 0 when afl-fuzz fuzzed for
# <seconds>, or stopped early on a crash it saved, which the workflow's crash
# report then fails on. Findings are in <output directory>/default/crashes
# and hangs either way.
#
# - AFL_SKIP_CPUFREQ and AFL_NO_AFFINITY are the usual accommodations for
#   shared runners. AFL_MAP_SIZE and AFL_MAPSIZE are cleared, so the map is
#   the 1 MiB that scripts/afl/build-afl.sh builds the tools with, and
#   AFL_AUTORESUME too: each campaign starts fresh.
# - AFL_CRASH_EXITCODE=1: AFL counts only a signal as a crash, and the
#   harness reports what the engine does not model (an escaped exception, an
#   access violation FPC turned into one) by catching it and exiting 1, so
#   without it no such finding reaches crashes/.
# - -t 5000: the harness's own budget is 1000 ms and a harness timeout is a
#   normal exit-0 outcome, so AFL's exec threshold sits well above it;
#   otherwise process overhead around a legitimate slow input lands it in
#   hangs/ and fails the job falsely.
# - -V: afl-fuzz stops after <seconds> of fuzzing, not counting the seeds'
#   calibration, and writes its final stats. It does not reliably exit on a
#   SIGTERM: run 31771115536 sat 43 more minutes on one until the job's
#   60-minute cap cancelled it, and v5.03c ignored timeout's every time (run
#   45623922253). timeout -k 60 stays as a backstop 30 minutes past
#   <seconds>, for the calibration: the engine's instrumented harness takes
#   about 19 minutes over the script corpus's 2,064 seeds on a runner at -O-
#   (run 45852001706), and less at the -O4 the workflow builds it with.
#   afl-fuzz still running then has stalled, which fails the run.
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
# A relative directory is named from ./, so no command below takes one that
# starts with - for an option.
case $out in
  /*) ;;
  *) out=./$out ;;
esac

# The workflow's dispatch input arrives as text. Its digits are checked
# before any arithmetic, which would read 0900 as octal and wrap a long
# number around.
digits=${seconds#"${seconds%%[!0]*}"}
if ! [[ $seconds =~ ^[0-9]+$ ]] || [ -z "$digits" ] || [ ${#digits} -gt 4 ] ||
  ((10#$digits > 3600)); then
  echo "::error::fuzz seconds must be a whole number from 1 to 3600, not '$seconds'" >&2
  exit 2
fi
seconds=$((10#$digits))
deadline=$((seconds + 1800))

# A fresh campaign only: on a resume afl-fuzz's run_time includes the earlier
# run, so the check below could not tell whether this one ran its course.
if [ "$seeds" = - ] || [ -e "$out/default" ]; then
  echo "::error::scripts/afl/fuzz.sh starts a fresh campaign: give it seeds and an output directory without a default/ campaign" >&2
  exit 2
fi

mkdir -p "$out"
status=0
started=$SECONDS
env -u AFL_MAP_SIZE -u AFL_MAPSIZE -u AFL_AUTORESUME \
  AFL_SKIP_CPUFREQ=1 AFL_NO_AFFINITY=1 AFL_BENCH_UNTIL_CRASH=1 AFL_CRASH_EXITCODE=1 \
  timeout -k 60 "$deadline" afl-fuzz -V "$seconds" -t 5000 \
  -i "$seeds" -o "$out" -- "$@" || status=$?
elapsed=$((SECONDS - started))
echo "afl-fuzz exit status: ${status} after ${elapsed}s"

# 124 and 137 at the deadline are the backstop; 137 before it is an outside
# kill, such as the OOM killer. Any other nonzero status is afl-fuzz not
# getting off the ground (bad arguments, a missing harness, 127 for a missing
# afl-fuzz). A 0 counts only when afl-fuzz's own stats show it fuzzed for
# <seconds>, or when it saved a crash: it also exits 0 when a signal or
# AFL_BENCH_JUST_ONE stops it early.
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

if [ -n "$(find "$out/default/crashes" -maxdepth 1 -name 'id:*' -print -quit 2> /dev/null)" ]; then
  exit 0
fi
fuzzed=$(awk -F' *: *' '$1 == "run_time" { print $2 }' < "$out/default/fuzzer_stats" 2> /dev/null || true)
if ! [[ $fuzzed =~ ^[0-9]+$ ]] || [ "$fuzzed" -lt "$seconds" ]; then
  echo "::error::afl-fuzz exited 0 after ${fuzzed:-an unknown number of} seconds of fuzzing, short of its ${seconds}s limit, and saved no crash"
  exit 1
fi
