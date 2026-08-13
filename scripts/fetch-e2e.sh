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
SERVER_PID=""

cleanup() {
  [ -n "$SERVER_PID" ] && kill "$SERVER_PID" 2>/dev/null || true
  rm -f "$TMPFILE" "$PORT_FILE"
}
trap cleanup EXIT

# --- Start test server ---

python3 scripts/fetch_test_server.py "$PORT_FILE" &
SERVER_PID=$!
for _ in $(seq 1 50); do
  [ -s "$PORT_FILE" ] && break
  sleep 0.1
done
PORT="$(cat "$PORT_FILE")"
BASE="http://127.0.0.1:${PORT}"

echo "=== fetch CLI end-to-end tests (server on port $PORT) ==="
echo ""

# fetch() refuses to run at all unless an allowlist is configured, so every
# invocation here has to name the test server's host. Without it the whole
# script fails with "fetch requires allowed hosts to be configured" long
# before reaching any assertion.
ALLOW_HOST="--allowed-host=127.0.0.1"

run_js() {
  echo "$1" > "$TMPFILE"
  "$LOADER" "$ALLOW_HOST" "$TMPFILE" --compat-asi 2>&1
}

# Same as run_js, but with extra loader flags before the script path, so the
# policy flags can be exercised against the same live server.
run_js_with() {
  local extra="$1"
  echo "$2" > "$TMPFILE"
  # $extra is a deliberate flag list, so word splitting is wanted here.
  # shellcheck disable=SC2086
  "$LOADER" "$ALLOW_HOST" $extra "$TMPFILE" --compat-asi 2>&1
}

check_with() {
  local desc="$1"
  local extra="$2"
  local expected_exit="$3"
  local source="$4"
  shift 4
  local expected_output="${1:-}"
  local actual
  local exit_code

  actual="$(run_js_with "$extra" "$source")" && exit_code=0 || exit_code=$?

  if [ "$exit_code" -ne "$expected_exit" ]; then
    echo "FAIL: $desc (exit code: expected $expected_exit, got $exit_code)"
    echo "  output: $actual"
    FAIL=$((FAIL + 1))
    return
  fi

  if [ -n "$expected_output" ]; then
    if echo "$actual" | grep -qF "$expected_output"; then
      PASS=$((PASS + 1))
    else
      echo "FAIL: $desc"
      echo "  expected to contain: $expected_output"
      echo "  actual: $actual"
      FAIL=$((FAIL + 1))
    fi
  else
    PASS=$((PASS + 1))
  fi
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

# --- Private-range policy (WP-2) ---
#
# The test server listens on loopback, so it is itself a private target. That
# makes it the honest fixture for this policy: with the flag off the request
# must still work (no behavior change for existing hosts), and with it on the
# very same request must be refused.

check_with "loopback reachable without the deny flag" "" 0 "
const r = await fetch('${BASE}/text')
console.log(r.status)
" "200"

check_with "loopback refused with --fetch-deny-private-ranges" \
  "--fetch-deny-private-ranges" 1 "
await fetch('${BASE}/text')
" "TypeError"

# Exits 0: the script catches the rejection, so the assertion is on the
# message, which must name the address the host actually resolved to.
check_with "private-range rejection names the resolved address" \
  "--fetch-deny-private-ranges" 0 "
try {
  await fetch('${BASE}/text')
} catch (e) {
  console.log(e.message)
}
" "127.0.0.1"

# A redirect hop must be resolved and validated exactly like the first
# request; validating only the initial target would leave the hole open.
#
# The initial request goes to loopback (allowed), so it is NOT rejected up
# front — the 302 then points at a private, off-allowlist address. Only per-hop
# revalidation can catch that, and the rejection must name the redirect target
# (10.255.255.1), not the initial host, proving the hop itself was checked. A
# regression that validates only the first request would instead attempt to
# connect to the redirect target and fail with a different, connect-level error.
#
# --timeout bounds that regression: the fetch connect timeout derives from the
# remaining execution budget (FetchManager: RequestTimeoutMilliseconds :=
# RemainingExecutionTimeoutMilliseconds), so a hard loader timeout caps an
# otherwise-unbounded blocking connect to 10.255.255.1:1 if per-hop validation
# regresses. In the passing case the hop is rejected before any connect, so the
# timeout never fires.
check_with "redirect hop is policy-checked, not just the initial request" \
  "--timeout=15000" 0 "
try {
  await fetch('${BASE}/redirect-external')
} catch (e) {
  console.log(e.message)
}
" "not allowed: 10.255.255.1"

# --- Response body cap (WP-2) ---

check_with "response under the cap succeeds" \
  "--fetch-max-response-bytes=1048576" 0 "
const r = await fetch('${BASE}/text')
console.log((await r.text()).trim())
" "hello world"

check_with "response over the cap is refused" \
  "--fetch-max-response-bytes=4" 1 "
await fetch('${BASE}/text')
" "TypeError"

echo ""
echo "=== Results: $PASS passed, $FAIL failed ==="

if [ "$FAIL" -gt 0 ]; then
  exit 1
fi
