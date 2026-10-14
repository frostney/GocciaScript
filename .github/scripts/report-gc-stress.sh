#!/usr/bin/env bash
# Opens or updates the one tracking issue for tests that fail only under the
# GC-stress build, from the report scripts/gc-stress.ts wrote.
#
# Usage: report-gc-stress.sh <report.md>
#
# Writes only when FILE_ISSUE is "true"; gc-stress.yml sets it for scheduled
# runs and default-branch runs of the upstream repository. Any other run is a
# dry run: it looks the tracking issue up (read-only) and prints what it would
# have done, so a branch run never files an issue.

set -euo pipefail

report="$1"
title="GC-stress run: tests fail only under the GC-stress build"

# The open tracking issue, matched on the exact title. Search narrows the
# list; the jq filter drops near-matches.
existing="$(gh issue list --repo "$GITHUB_REPOSITORY" --state open \
  --search "\"${title}\" in:title" --json number,title \
  --jq "[.[] | select(.title == \"${title}\")] | first | .number // empty")"

if [ -n "$existing" ]; then
  action="comment on #${existing}"
else
  action="open a new issue titled \"${title}\""
fi

if [ "${FILE_ISSUE:-false}" != "true" ]; then
  echo "Dry run (FILE_ISSUE=${FILE_ISSUE:-unset}, event ${GITHUB_EVENT_NAME:-unknown}, ref ${GITHUB_REF:-unknown}): would ${action}."
  if [ -n "${GITHUB_STEP_SUMMARY:-}" ]; then
    echo "Dry run: a scheduled or default-branch run would ${action}." >> "$GITHUB_STEP_SUMMARY"
  fi
  exit 0
fi

if [ -n "$existing" ]; then
  gh issue comment "$existing" --repo "$GITHUB_REPOSITORY" --body-file "$report"
  echo "Commented on #${existing}."
else
  body="$(mktemp)"
  {
    echo "The weekly GC-stress run (\`.github/workflows/gc-stress.yml\`) found tests that pass in an ordinary build but fail when the VM collects every 8 instructions. Each is likely a value that only a Pascal local holds while guest code runs."
    echo
    cat "$report"
    echo
    echo "Later runs comment here while this issue is open. Close it once these tests pass under stress; the next failing run then opens a new one."
  } > "$body"
  gh issue create --repo "$GITHUB_REPOSITORY" --title "$title" \
    --label bug --label engine --body-file "$body"
fi
