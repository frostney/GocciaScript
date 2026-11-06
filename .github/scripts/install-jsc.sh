#!/usr/bin/env bash
# Installs the JavaScriptCore shell (`jsc`) from the runner's Ubuntu archive for
# the AWFY and JetStream lanes, which run it with its JIT off (ADR 0178). The
# archive replaces superseded package versions, so the version is recorded
# rather than pinned: the driver writes it into each report's engine metadata,
# and this script prints it with the install time.
set -euo pipefail

package="libjavascriptcoregtk-bin"
started="$(date +%s)"
# The index is refreshed first, so that the parallel JetStream shards, whose
# reports the merge requires to name one JSC version, install the archive's
# current package rather than whatever each runner image cached.
sudo apt-get update
sudo apt-get install -y --no-install-recommends "$package"
finished="$(date +%s)"

version="$(dpkg-query -W -f='${Version}' "$package")"
echo "Installed $package $version in $((finished - started)) s"
jsc --useJIT=false -e 'print("JavaScriptCore " + (1 + 2))'
