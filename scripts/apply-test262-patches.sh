#!/usr/bin/env bash
# scripts/apply-test262-patches.sh SUITE_DIR: applies scripts/test262-patches/*.patch to a test262 checkout at the
# pinned SHA (scripts/test262-suite-sha.txt). Each patch is an upstream tc39/test262 pull request, named after it,
# that corrects a test the pinned suite still has wrong against the current specification; GocciaScript is held to the
# corrected test. Once the pin moves past the pull request's merge, the patch no longer applies and this script
# fails, naming it: delete the patch then.
set -euo pipefail
suite=$1
here=$(cd "$(dirname "$0")" && pwd)
shopt -s nullglob
for patch in "$here"/test262-patches/*.patch; do
  if ! git -C "$suite" apply --check "$patch" 2>/dev/null; then
    if git -C "$suite" apply --check --reverse "$patch" 2>/dev/null; then
      echo "already applied: $(basename "$patch")"
      continue
    fi
    echo "::error::$(basename "$patch") does not apply to the pinned test262: if the pin includes its pull request, delete it" >&2
    exit 1
  fi
  git -C "$suite" apply "$patch"
  echo "applied: $(basename "$patch")"
done
