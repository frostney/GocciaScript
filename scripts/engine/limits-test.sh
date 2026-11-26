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

# case NAME FLAGS EXPECTED-OUTPUT SOURCE: runs SOURCE with FLAGS, compares the whole output and the exit code
# (1 when EXPECTED starts with "Fatal error", 0 otherwise)
check() {
  local name=$1 flags=$2 want=$3 src=$4 got rc want_rc=0
  printf '%s\n' "$src" > "$D/$name.js"
  got=$(timeout 20 "$ENGINE" $flags "$D/$name.js" 2>&1); rc=$?
  case $want in "Fatal error"*) want_rc=1 ;; esac
  if [ "$got" != "$want" ] || [ $rc -ne $want_rc ]; then
    echo "FAIL $name ($flags): exit $rc, output: $got (want exit $want_rc: $want)"
    fails=$((fails + 1))
  fi
}

T='Fatal error: file timed out after 200ms'
check loop --timeout=200 "$T" 'while (true) {}'
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

check max-stack --max-stack=100 '100 true' 'let d = 0; function r() { d++; r(); } try { r(); } catch (e) { console.log(d, e instanceof RangeError); }'

[ $fails -eq 0 ] && echo "limits: all passed" || { echo "limits: $fails failed"; exit 1; }
