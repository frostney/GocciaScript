#!/usr/bin/env bash
# Run against the unit cache of a built engine target (default: --prod engine).
set -euo pipefail

unit_dir=${1:-build/compiled/targets/engine}
fixture_dir=$(mktemp -d build/aborted-run.XXXXXX)
server_pid=''
cleanup() {
  if [[ -n "$server_pid" ]]; then kill "$server_pid" 2>/dev/null || true; fi
  rm -rf "$fixture_dir"
}
trap cleanup EXIT

fpc -vw-n-h-i-l-d-u-t-p-c-x- "-Fu$unit_dir" "-o$fixture_dir/aborted-run-test" scripts/engine/aborted-run-test.pas
python3 scripts/fetch_test_server.py "$fixture_dir/port" &
server_pid=$!
for _ in $(seq 1 300); do
  if [[ -s "$fixture_dir/port" ]]; then break; fi
  if ! kill -0 "$server_pid" 2>/dev/null; then break; fi
  sleep 0.1
done
if [[ ! -s "$fixture_dir/port" ]]; then
  echo "The fetch test fixture did not start" >&2
  exit 1
fi

# The second URL is a request the server holds for a minute: a server that does not answer.
port=$(cat "$fixture_dir/port")
timeout 20 "$fixture_dir/aborted-run-test" "http://127.0.0.1:$port/text" "http://127.0.0.1:$port/sleep/60000/hang"
