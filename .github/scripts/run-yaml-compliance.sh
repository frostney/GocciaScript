#!/usr/bin/env bash

# Checks out yaml/yaml-test-suite at tests/compliance/yaml-test-suite.pin and
# runs GocciaYAMLComplianceRunner over it, which exits non-zero for any case
# that fails outside tests/compliance/yaml-test-suite.known-failures and for
# any listed case that passes. pr.yml and ci.yml both run this script, so the
# pull-request gate and the full CI run the same check.

set -euo pipefail

if [ "$#" -ne 3 ]; then
  echo "Usage: $0 <build-dir> <checkout-parent-dir> <report-path>" >&2
  exit 2
fi

build_dir="$1"
suite_dir="$2/yaml-test-suite"
report="$3"

pin="$(tr -d '\r\n' < tests/compliance/yaml-test-suite.pin)"
git init -q "$suite_dir"
# The cases are byte-exact inputs: keep Windows' Git from turning their line
# feeds into CRLF on checkout.
git -C "$suite_dir" config core.autocrlf false
git -C "$suite_dir" remote add origin https://github.com/yaml/yaml-test-suite.git
git -C "$suite_dir" fetch -q --depth 1 origin "$pin"
git -C "$suite_dir" checkout -q --detach "$pin"

runner="$build_dir/GocciaYAMLComplianceRunner"
if [ -f "$runner.exe" ]; then
  runner="$runner.exe"
fi
"$runner" --suite-dir="$suite_dir" --jobs=4 --output="$report"
