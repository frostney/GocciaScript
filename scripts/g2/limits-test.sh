#!/bin/bash
# scripts/g2/limits-test.sh [G2]: the execution limits of the G2 host (--timeout, --max-instructions, --max-stack).
# A spent limit aborts the run whatever the script catches: no catch or finally runs, a promise's rejection
# handler does not run, and the host reports it as GocciaRunner does. Prints one line per failing case and exits 1
# when any fails.
set -u
G2=${1:-build/g2/g2}
D=$(mktemp -d)
trap 'rm -rf "$D"' EXIT
fails=0

# case NAME FLAGS EXPECTED-OUTPUT SOURCE: runs SOURCE with FLAGS, compares the whole output and the exit code
# (1 when EXPECTED starts with "Fatal error", 0 otherwise)
check() {
  local name=$1 flags=$2 want=$3 src=$4 got rc want_rc=0
  printf '%s\n' "$src" > "$D/$name.js"
  got=$(timeout 20 "$G2" $flags "$D/$name.js" 2>&1); rc=$?
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
race=$(seq 1 200 | xargs -P 16 -I{} sh -c "timeout 20 \"$G2\" --timeout=50 \"$D/race.js\" > /dev/null 2>&1; echo \$?" | grep -cv '^1$')
[ "$race" -eq 0 ] || { echo "FAIL timeout-race: $race of 200 runs did not abort"; fails=$((fails + 1)); }
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
while [ $((hi - lo)) -gt 1 ]; do m=$(((lo + hi) / 2)); if "$G2" --max-instructions=$m "$D/sum.js" > /dev/null 2>&1; then hi=$m; else lo=$m; fi; done
for k in 1 2 3; do
  "$G2" --max-instructions=$hi "$D/sum.js" > /dev/null 2>&1 || { echo "FAIL instr-exact: $hi passed once, then failed"; fails=$((fails + 1)); break; }
  "$G2" --max-instructions=$((hi - 1)) "$D/sum.js" > /dev/null 2>&1 && { echo "FAIL instr-exact: $((hi - 1)) failed once, then passed"; fails=$((fails + 1)); break; }
done

check max-stack --max-stack=100 '100 true' 'let d = 0; function r() { d++; r(); } try { r(); } catch (e) { console.log(d, e instanceof RangeError); }'

[ $fails -eq 0 ] && echo "limits: all passed" || { echo "limits: $fails failed"; exit 1; }
