#!/usr/bin/env bash
# Run against the unit cache of a built engine target (default: --prod engine).
set -euo pipefail

unit_dir=${1:-build/compiled/targets/engine}
fixture_dir=$(mktemp -d build/script-artifact-imports.XXXXXX)
trap 'rm -rf "$fixture_dir"' EXIT

fpc -vw-n-h-i-l-d-u-t-p-c-x- "-Fu$unit_dir" "-o$fixture_dir/script-artifact-imports-test" \
  scripts/engine/script-artifact-imports-test.pas
mkdir "$fixture_dir/real" "$fixture_dir/link"
printf 'export const who = "real";\n' > "$fixture_dir/real/sib.mjs"
printf 'export const who = "link";\n' > "$fixture_dir/link/sib.mjs"
ln -s ../real/standalone.gbc "$fixture_dir/link/standalone.gbc"
timeout 20 "$fixture_dir/script-artifact-imports-test" "$fixture_dir"
