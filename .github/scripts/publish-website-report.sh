#!/usr/bin/env bash

set -euo pipefail

if [ -z "${BLOB_READ_WRITE_TOKEN:-}" ]; then
  echo "BLOB_READ_WRITE_TOKEN is not configured; skipping report publish."
  exit 0
fi

cd "$(dirname "${BASH_SOURCE[0]}")/../../website"

# RUNNER_TEMP is fresh for each CI job. Multiple publishers in that job share
# one dependency installation; local invocations always check the lockfile.
installed="${RUNNER_TEMP:+$RUNNER_TEMP/goccia-website-publisher-installed}"
if [ -z "$installed" ] || [ ! -f "$installed" ]; then
  bun install --frozen-lockfile
  if [ -n "$installed" ]; then
    touch "$installed"
  fi
fi

exec bun "$@"
