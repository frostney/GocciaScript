#!/bin/bash
# scripts/engine/limits-test.sh [GOCCIA]: the execution limits of the engine host (--timeout, --max-instructions, --max-stack,
# --max-memory).
# A spent limit aborts the run whatever the script catches: no catch or finally runs, a promise's rejection
# handler does not run, and the host reports it as GocciaRunner does. Prints one line per failing case and exits 1
# when any fails.
set -u
ENGINE=${1:-build/engine/goccia}
D=$(mktemp -d)
trap 'rm -rf "$D"' EXIT
fails=0

# check NAME FLAGS EXPECTED-OUTPUT SOURCE [WATCHDOG-SECONDS]: compares the whole output and exit code
# (1 when EXPECTED starts with "Fatal error", 0 otherwise)
check() {
  local name=$1 flags=$2 want=$3 src=$4 watchdog=${5:-20} got rc want_rc=0
  printf '%s\n' "$src" > "$D/$name.js"
  got=$(timeout "$watchdog" "$ENGINE" $flags "$D/$name.js" 2>&1); rc=$?
  case $want in "Fatal error"*) want_rc=1 ;; esac
  if [ "$got" != "$want" ] || [ $rc -ne $want_rc ]; then
    echo "FAIL $name ($flags): exit $rc, output: $got (want exit $want_rc: $want)"
    fails=$((fails + 1))
  fi
}

check typed-fill-budget --max-instructions=50 'Fatal error: Execution exceeded instruction limit of 50' \
  'const a = new Int32Array(256); a.fill(42); console.log(a[13]);'
# typed-array element loops: one step per element. Single-loop cases run 100,000 elements under a cap of 200, and the
# control shows the script's own bytecode fits under it. Cases with several loops have a cap below their total but
# above the total less any one loop's elements (measured: filter 30,017, sort 12,301, iterables about 20,017, of
# 4,017), so a loop that stopped polling would let the case finish.
TA_CAP=--max-instructions=200
TA_LIMIT='Fatal error: Execution exceeded instruction limit of 200'
check typed-loop-control "$TA_CAP" '42' 'const a = new Int32Array(1); a.indexOf(1); console.log(42);'
check typed-from-array-like-budget "$TA_CAP" "$TA_LIMIT" 'Int32Array.from({ length: 100000 }); console.log(42);'
check typed-map-budget "$TA_CAP" "$TA_LIMIT" 'const a = new Int32Array(100000); a.map(Math.abs); console.log(42);'
check typed-for-each-budget "$TA_CAP" "$TA_LIMIT" 'const a = new Int32Array(100000); a.forEach(Math.abs); console.log(42);'
check typed-every-budget "$TA_CAP" "$TA_LIMIT" 'const a = new Int32Array(100000); a.every(Number.isInteger); console.log(42);'
check typed-reduce-budget "$TA_CAP" "$TA_LIMIT" 'const a = new Int32Array(100000); a.reduce(Math.abs); console.log(42);'
check typed-index-of-budget "$TA_CAP" "$TA_LIMIT" 'const a = new Int32Array(100000); a.indexOf(1); console.log(42);'
check typed-last-index-of-budget "$TA_CAP" "$TA_LIMIT" 'const a = new Int32Array(100000); a.lastIndexOf(1); console.log(42);'
check typed-includes-budget "$TA_CAP" "$TA_LIMIT" 'const a = new Int32Array(100000); a.includes(1); console.log(42);'
check typed-join-budget "$TA_CAP" "$TA_LIMIT" 'const a = new Int32Array(100000); a.join(); console.log(42);'
check typed-reverse-budget "$TA_CAP" "$TA_LIMIT" 'const a = new Int32Array(100000); a.reverse(); console.log(42);'
check typed-set-convert-budget "$TA_CAP" "$TA_LIMIT" 'const a = new Int32Array(100000); new Float64Array(100000).set(a); console.log(42);'
check typed-set-array-like-budget "$TA_CAP" "$TA_LIMIT" 'const a = new Int32Array(100000); a.set({ length: 100000 }); console.log(42);'
check typed-slice-same-type-budget "$TA_CAP" "$TA_LIMIT" 'const a = new Int32Array(100000); a.slice(); console.log(42);'
check typed-to-reversed-budget "$TA_CAP" "$TA_LIMIT" 'const a = new Int32Array(100000); a.toReversed(); console.log(42);'
check typed-with-budget "$TA_CAP" "$TA_LIMIT" 'const a = new Int32Array(100000); a.with(0, 1); console.log(42);'
check typed-construct-convert-budget "$TA_CAP" "$TA_LIMIT" 'const a = new Int32Array(100000); new Float64Array(a); console.log(42);'
check typed-construct-bigint-convert-budget "$TA_CAP" "$TA_LIMIT" 'new BigUint64Array(new BigInt64Array(100000)); console.log(42);'
check typed-slice-convert-budget "$TA_CAP" "$TA_LIMIT" \
  'class G extends Float64Array { static get [Symbol.species]() { return Int32Array; } } new G(100000).slice(); console.log(42);'
check typed-filter-budget --max-instructions=25000 'Fatal error: Execution exceeded instruction limit of 25000' \
  'const a = new Int32Array(10000); a.fill(1); a.filter(Boolean); console.log(42);'
check typed-sort-budget --max-instructions=11800 'Fatal error: Execution exceeded instruction limit of 11800' \
  'const a = new Int32Array(1024); a.sort(); console.log(42);'
check typed-to-sorted-budget --max-instructions=11800 'Fatal error: Execution exceeded instruction limit of 11800' \
  'const a = new Int32Array(1024); a.toSorted(); console.log(42);'
check typed-construct-iterable-budget --max-instructions=15000 'Fatal error: Execution exceeded instruction limit of 15000' \
  'const a = new Int32Array(10000); new Int32Array(a.values()); console.log(42);'
check typed-from-iterable-budget --max-instructions=15000 'Fatal error: Execution exceeded instruction limit of 15000' \
  'const a = new Int32Array(10000); Int32Array.from(a); console.log(42);'
check typed-of-budget --max-instructions=3000 'Fatal error: Execution exceeded instruction limit of 3000' \
  'const b = new Array(2000); Int32Array.of(...b); console.log(42);'
# own-key loops: one step per key listed, and one per key a native then walks. A String's characters are its keys.
# Single-loop cases list 100,000 keys under a cap of 200. Cases with two loops have a cap below their total but above
# the total less either loop's keys (measured: 10,000 keys walked once more 20,013 to 20,016, the array cases 30,019
# and 30,020 with fill's 10,000), so a loop that stopped polling would let the case finish.
check own-keys-control "$TA_CAP" '42' 'Object.keys("x"); console.log(42);'
check own-keys-budget "$TA_CAP" "$TA_LIMIT" 'Object.keys("x".repeat(100000)); console.log(42);'
check own-names-budget "$TA_CAP" "$TA_LIMIT" 'Object.getOwnPropertyNames("x".repeat(100000)); console.log(42);'
check for-in-keys-budget "$TA_CAP" "$TA_LIMIT" 'for (const k in "x".repeat(100000)) break; console.log(42);'
K_CAP=--max-instructions=15000
K_LIMIT='Fatal error: Execution exceeded instruction limit of 15000'
check own-values-budget "$K_CAP" "$K_LIMIT" 'Object.values("x".repeat(10000)); console.log(42);'
check own-entries-budget "$K_CAP" "$K_LIMIT" 'Object.entries("x".repeat(10000)); console.log(42);'
check own-assign-budget "$K_CAP" "$K_LIMIT" 'Object.assign({}, "x".repeat(10000)); console.log(42);'
check own-spread-budget "$K_CAP" "$K_LIMIT" '({ ..."x".repeat(10000) }); console.log(42);'
check own-descriptors-budget "$K_CAP" "$K_LIMIT" 'Object.getOwnPropertyDescriptors("x".repeat(10000)); console.log(42);'
check own-array-spread-budget --max-instructions=25000 'Fatal error: Execution exceeded instruction limit of 25000' \
  'const a = new Array(10000).fill(0); ({ ...a }); console.log(42);'
check own-freeze-budget --max-instructions=25000 'Fatal error: Execution exceeded instruction limit of 25000' \
  'const a = new Array(10000).fill(0); Object.freeze(a); console.log(42);'
# measured: a for-in over a prototype's keys 20,025, object rest 20,020, a proxy's keys 20,022 (names too) and its
# JSON 30,022, defineProperties' two loops 40,023 and isFrozen 50,024, each with fill's 10,000 where it has one
check own-for-in-proto-budget "$K_CAP" "$K_LIMIT" \
  'for (const k in Object.create(new String("x".repeat(10000)))) break; console.log(42);'
check own-rest-budget "$K_CAP" "$K_LIMIT" 'const { a, ...r } = "x".repeat(10000); console.log(42);'
check own-proxy-keys-budget "$K_CAP" "$K_LIMIT" \
  'Object.keys(new Proxy(new String("x".repeat(10000)), {})); console.log(42);'
check own-proxy-names-budget "$K_CAP" "$K_LIMIT" \
  'Object.getOwnPropertyNames(new Proxy(new String("x".repeat(10000)), {})); console.log(42);'
check own-proxy-json-budget --max-instructions=25000 'Fatal error: Execution exceeded instruction limit of 25000' \
  'JSON.stringify(new Proxy(new String("x".repeat(10000)), {})); console.log(42);'
check own-define-properties-budget --max-instructions=35000 'Fatal error: Execution exceeded instruction limit of 35000' \
  'const d = new Array(10000).fill({}); Object.defineProperties({}, d); console.log(42);'
check own-is-frozen-budget --max-instructions=45000 'Fatal error: Execution exceeded instruction limit of 45000' \
  'const a = Object.freeze(new Array(10000).fill(0)); Object.isFrozen(a); console.log(42);'

T='Fatal error: file timed out after 200ms'
check loop --timeout=200 "$T" 'while (true) {}'
check atomics-wait --timeout=200 "$T" \
  'const a = new Int32Array(new SharedArrayBuffer(4)); try { Atomics.wait(a, 0, 0, 15000); } catch { console.log("caught"); }' 5
# a huge finite timeout waits as an infinite one, until the run's deadline, rather than returning at once
check atomics-wait-huge-timeout --timeout=200 "$T" \
  'Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, 1e300); console.log("returned");'
check atomics-wait-max-timeout --timeout=200 "$T" \
  'Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, Number.MAX_VALUE); console.log("returned");'
# a long finite timeout past 32 bits of milliseconds (about 49.7 days) is a real wait, not one that wraps to zero
check atomics-wait-long-timeout --timeout=200 "$T" \
  'Atomics.wait(new Int32Array(new SharedArrayBuffer(4)), 0, 0, 2 ** 32); console.log("returned");'
check atomics-guest-timeout --timeout=2000 'timed-out' \
  'const a = new Int32Array(new SharedArrayBuffer(4)); console.log(Atomics.wait(a, 0, 0, 10));' 5
# the process exits cleanly after an asynchronous Atomics.waitAsync, settled or still pending at the run's deadline
check atomics-wait-async-exit --timeout=2000 'timed-out' \
  'Atomics.waitAsync(new Int32Array(new SharedArrayBuffer(4)), 0, 0, 10).value.then(r => console.log(r));' 5
check atomics-wait-async-pending-exit --timeout=200 "$T" \
  'Atomics.waitAsync(new Int32Array(new SharedArrayBuffer(4)), 0, 0, 60000).value.then(r => console.log(r));'
check do-while --timeout=200 "$T" 'let n = 0; do { n++; } while (true);'
check catch-finally --timeout=200 "$T" \
  'while (true) { try { for (;;) {} } catch (e) { console.log("caught"); } finally { console.log("finally"); } }'
check recursion --timeout=200 "$T" 'function f() { try { f(); } catch { f(); } } f();'
check native-callback --timeout=200 "$T" \
  'try { [1].forEach(() => { for (;;) {} }); } catch (e) { console.log("caught"); }'
check promise-chain --timeout=200 "$T" 'const f = () => Promise.resolve().then(f); f();'
check promise-catch --timeout=200 "$T" \
  'Promise.resolve().then(() => { for (;;) {} }).catch(() => console.log("caught")).finally(() => console.log("finally"));'
check async-loop --timeout=200 "$T" 'async function f() { for (;;) await null; } f().catch(() => console.log("rejected"));'
check generator --timeout=200 "$T" 'function* g() { for (;;) yield 1; } try { for (const x of g()) {} } finally { console.log("finally"); }'
check getter --timeout=200 "$T" 'try { JSON.stringify({ get x() { for (;;) {} } }); } catch { console.log("caught"); }'
# the timer's interrupt against a call in flight, many times at once: a run that sees the frame bound drop before
# Interrupt is set throws a catchable RangeError, the promise job swallows it and the run exits 0
printf '%s\n' 'const f = () => Promise.resolve().then(f); f();' > "$D/race.js"
race=$(seq 1 200 | xargs -P 16 -I{} sh -c "timeout 20 \"$ENGINE\" --timeout=50 \"$D/race.js\" > /dev/null 2>&1; echo \$?" | grep -cv '^1$')
[ "$race" -eq 0 ] || { echo "FAIL timeout-race: $race of 200 runs did not abort"; fails=$((fails + 1)); }
# BigInt work whose time grows faster than its operands stops at the deadline: multiplication, division and radix
# conversion of a value of a few million bits, and parsing a long digit string
check big-square --timeout=200 "$T" 'let b = 3n; for (;;) b = b * b + 1n;'
check big-divide --timeout=200 "$T" 'const b = 3n ** 3000000n; for (;;) b / (b >> 4000000n);'
check big-string --timeout=200 "$T" 'String(7n ** 3000000n);'
check big-parse --timeout=200 "$T" 'BigInt("7".repeat(3000000));'
# a native that lists millions of own keys stops at the deadline: Object.entries of a String object of 4,000,000
# characters runs for about a second with no limit, all of it in one call
check own-entries-timeout --timeout=200 "$T" 'Object.entries("x".repeat(4000000)); console.log("returned");'
check under-timeout --timeout=5000 '499999500000' 'let s = 0; for (let i = 0; i < 1e6; i++) s += i; console.log(s);'

I='Fatal error: Execution exceeded instruction limit of 1000'
check instr-loop --max-instructions=1000 "$I" 'while (true) {}'
check instr-jobs --max-instructions=1000 "$I" \
  'let n = 0; const f = () => { n++; if (n < 100000) return Promise.resolve().then(f); console.log("done"); }; f();'
check instr-microtasks --max-instructions=1000 "$I" \
  'let n = 0; const f = () => { n++; if (n < 100000) { queueMicrotask(f); return; } console.log("done"); }; f();'
check instr-under --max-instructions=100000 '4950' 'let s = 0; for (let i = 0; i < 100; i++) s += i; console.log(s);'
# the count is exact: the same script needs the same budget every run
SUM='let s = 0; for (let i = 0; i < 100; i++) s += i; console.log(s);'
printf '%s\n' "$SUM" > "$D/sum.js"
lo=1; hi=100000
while [ $((hi - lo)) -gt 1 ]; do m=$(((lo + hi) / 2)); if "$ENGINE" --max-instructions=$m "$D/sum.js" > /dev/null 2>&1; then hi=$m; else lo=$m; fi; done
for k in 1 2 3; do
  "$ENGINE" --max-instructions=$hi "$D/sum.js" > /dev/null 2>&1 || { echo "FAIL instr-exact: $hi passed once, then failed"; fails=$((fails + 1)); break; }
  "$ENGINE" --max-instructions=$((hi - 1)) "$D/sum.js" > /dev/null 2>&1 && { echo "FAIL instr-exact: $((hi - 1)) failed once, then passed"; fails=$((fails + 1)); break; }
done

# --max-memory (ADR 0185): a full collection that leaves more live than the limit refuses at the next back edge or
# call with a catchable RangeError; a script that refuses to free anything after eight refusals in a row ends as a
# spent limit does (the message names the live bytes, which vary). Compiled loops poll for it at their back edges.
R='Uncaught RangeError: Allocation failed: GC heap size exceeds the configured memory limit'
F='Fatal error: Live heap of [0-9]* bytes exceeds the memory limit of 33554432 bytes'
checkm() {
  local name=$1 want=$2 wantrc=$3 src=$4 got rc
  printf '%s\n' "$src" > "$D/$name.js"
  got=$(timeout 60 "$ENGINE" --max-memory=32MiB "$D/$name.js" 2>&1); rc=$?
  if ! printf '%s\n' "$got" | grep -qx "$want" || [ $rc -ne "$wantrc" ]; then
    echo "FAIL $name (--max-memory=32MiB): exit $rc, output: $got (want exit $wantrc: $want)"
    fails=$((fails + 1))
  fi
}
GROW='const a = []; while (true) a.push({ x: a.length });'
checkm mem-grow "$R" 1 "$GROW"
checkm mem-catch 'finally' 0 "try { $GROW } catch (e) { console.log('caught'); } finally { console.log('finally'); }"
checkm mem-promise 'caught' 0 "Promise.resolve().then(() => { $GROW }).catch(() => console.log('caught'));"
checkm mem-native "$R" 1 'const a = []; let i = 0; while (true) { a.push(i); i++; }'
checkm mem-strings "$R" 1 'const a = []; for (let i = 0; ; i++) a.push("s" + i);'
checkm mem-swallow "$F" 1 'const a = []; while (true) { try { while (true) a.push({ x: 1 }); } catch (e) {} }'
checkm mem-recover 'recovered' 0 "let a = []; try { $GROW } catch (e) { a = null; } const b = []; for (let i = 0; i < 1000; i++) b.push({ i }); console.log('recovered');"
check mem-garbage --max-memory=32MiB '1999999000000' \
  'let n = 0; for (let i = 0; i < 2e6; i++) { const o = { x: i }; n += o.x; } console.log(n);'
check mem-reserve --max-memory=32MiB 'RangeError' \
  'try { new ArrayBuffer(64 * 1024 * 1024); } catch (e) { console.log(e.name); }'
# the index keys of 2,000,000 characters pass the limit while Object.entries lists them: the refusal reaches the
# script at the next key, where a native that did not poll ran a full collection for every nursery it filled
check mem-own-entries --max-memory=32MiB 'RangeError' \
  'try { Object.entries("x".repeat(2000000)); console.log("returned"); } catch (e) { console.log(e.name); }'

check max-stack --max-stack=100 '100 true' 'let d = 0; function r() { d++; r(); } try { r(); } catch (e) { console.log(d, e instanceof RangeError); }'

# a file that does not exist, or none: an error and its exit status, not an unhandled exception
got=$("$ENGINE" "$D/missing.js" 2>&1); rc=$?
[ "$got" = "Error: Path not found: $D/missing.js" ] && [ $rc -eq 1 ] ||
  { echo "FAIL missing-file: exit $rc, output: $got"; fails=$((fails + 1)); }
got=$("$ENGINE" 2>&1); rc=$?
[ "$got" = "Usage: goccia [options] <file>" ] && [ $rc -eq 2 ] ||
  { echo "FAIL no-file: exit $rc, output: $got"; fails=$((fails + 1)); }

[ $fails -eq 0 ] && echo "limits: all passed" || { echo "limits: $fails failed"; exit 1; }
