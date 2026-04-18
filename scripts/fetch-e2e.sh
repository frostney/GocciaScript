#!/usr/bin/env bash
#
# End-to-end CLI tests for fetch().
# Spins up a local HTTP server, makes real requests, validates responses.
#
# Usage: bash scripts/fetch-e2e.sh
# Requires: ./build/GocciaScriptLoader, python3

set -euo pipefail

LOADER="./build/GocciaScriptLoader"
PASS=0
FAIL=0
TMPFILE="/tmp/goccia-fetch-e2e-$$.js"
PORT_FILE="/tmp/goccia-fetch-port-$$.txt"

cleanup() {
  [ -f /tmp/goccia-fetch-server.pid ] && kill "$(cat /tmp/goccia-fetch-server.pid)" 2>/dev/null || true
  rm -f "$TMPFILE" "$PORT_FILE" /tmp/goccia-fetch-server.pid
}
trap cleanup EXIT

# --- Start test server ---

python3 scripts/fetch_test_server.py "$PORT_FILE" &
sleep 0.5
PORT="$(cat "$PORT_FILE")"
BASE="http://127.0.0.1:${PORT}"

echo "=== fetch CLI end-to-end tests (server on port $PORT) ==="
echo ""

run_js() {
  echo "$1" > "$TMPFILE"
  "$LOADER" "$TMPFILE" --asi 2>&1
}

check() {
  local desc="$1"
  local expected_exit="$2"
  local source="$3"
  shift 3
  local expected_output="${1:-}"

  set +e
  actual="$(run_js "$source")"
  exit_code=$?
  set -e

  if [ "$exit_code" -ne "$expected_exit" ]; then
    echo "FAIL: $desc (exit code: expected $expected_exit, got $exit_code)"
    echo "  output: $actual"
    FAIL=$((FAIL + 1))
    return
  fi

  if [ -n "$expected_output" ]; then
    if echo "$actual" | grep -qF "$expected_output"; then
      echo "PASS: $desc"
      PASS=$((PASS + 1))
    else
      echo "FAIL: $desc (output mismatch)"
      echo "  expected to contain: $expected_output"
      echo "  actual: $actual"
      FAIL=$((FAIL + 1))
    fi
  else
    echo "PASS: $desc"
    PASS=$((PASS + 1))
  fi
}

# --- GET text ---
check "GET /text returns 200 with body" 0 "
const r = await fetch('${BASE}/text')
const t = await r.text()
console.log(r.status, r.ok, t)
" "200 true hello world"

# --- GET JSON ---
check "Response.json() parses body" 0 "
const r = await fetch('${BASE}/json')
const data = await r.json()
console.log(data.method, data.url)
" "GET /json"

# --- Custom headers ---
check "custom headers are sent" 0 "
const r = await fetch('${BASE}/echo-headers', {
  headers: { 'X-Goccia-Test': 'hello123' },
})
const data = await r.json()
const val = data.headers['X-Goccia-Test'] || data.headers['x-goccia-test']
console.log(val)
" "hello123"

# --- HEAD ---
check "HEAD returns empty body" 0 "
const r = await fetch('${BASE}/', { method: 'HEAD' })
const t = await r.text()
console.log(r.status, t.length)
" "200 0"

# --- Response headers ---
check "response headers.get returns content-type" 0 "
const r = await fetch('${BASE}/text')
console.log(r.headers.get('content-type'))
" "text/plain"

# --- arrayBuffer ---
check "arrayBuffer() has correct byteLength" 0 "
const r = await fetch('${BASE}/text')
const buf = await r.arrayBuffer()
console.log(buf instanceof ArrayBuffer, buf.byteLength)
" "true 11"

# --- bodyUsed ---
check "bodyUsed lifecycle" 0 "
const r = await fetch('${BASE}/text')
const before = r.bodyUsed
await r.text()
console.log(before, r.bodyUsed)
" "false true"

# --- Redirect ---
check "302 redirect is followed" 0 "
const r = await fetch('${BASE}/redirect')
const data = await r.json()
console.log(r.status, r.redirected, data.method)
" "200 true GET"

# --- Non-200 status ---
check "404 returns ok=false" 0 "
const r = await fetch('${BASE}/status/404')
console.log(r.status, r.ok)
" "404 false"

check "500 returns ok=false" 0 "
const r = await fetch('${BASE}/status/500')
console.log(r.status, r.ok)
" "500 false"

# --- Method restriction ---
check "POST is rejected" 1 \
  "fetch('${BASE}/', { method: 'POST' })" \
  "TypeError"

echo ""
echo "=== Results: $PASS passed, $FAIL failed ==="

if [ "$FAIL" -gt 0 ]; then
  exit 1
fi
