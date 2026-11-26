// try / catch / finally in compiled code (handlers pushed and popped by it, throws unwound by the interpreter to
// them): throws from the code, from compiled callees, natives and getters; nested handlers; finally with return,
// break and continue; labeled breaks out of a try; returns from inside a try in a callee; for-of loops left by
// break, throw and return (the iterator's return called, or absent); generators with try / finally and for-of;
// throws crossing frames with handlers of their own in between
let bad = 0;
function check(what, got, want) { if (!Object.is(got, want)) { bad++; console.log('FAIL', what, got, want); } }

function thrower(i) { if (i % 3 === 0) throw new RangeError('r' + i); return i; }
const getterThrows = { get v() { throw new TypeError('g'); } };
function catches(n) {
  let c = 0, s = 0, f = 0;
  for (let i = 0; i < n; i++) {
    try { s += thrower(i); } catch (e) { if (e instanceof RangeError) c++; } finally { f++; }
    try { JSON.parse(i % 5 ? '1' : '{'); } catch (e) { c += 10; }
    try { if (i % 7 === 0) getterThrows.v; } catch (e) { c += 100; }
  }
  return [c, s, f].join();
}
function nested(n) {
  let log = 0;
  for (let i = 0; i < n; i++) {
    try {
      try { if (i & 1) throw 1; log += 1; } finally { log += 10; }
    } catch (e) { log += 100; }
  }
  return log;
}
function finallyFlow(n) {
  let s = 0;
  outer: for (let i = 0; i < n; i++) {
    for (let j = 0; j < 4; j++) {
      try {
        if (j === 1) continue;
        if (j === 2 && i % 2) break;
        if (j === 3 && i % 5 === 0) continue outer;
        s += j;
      } finally { s += 100; }
    }
  }
  return s;
}
function retInTry(i) { try { if (i & 1) return 'odd'; } finally { retInTry.f++; } return 'even'; }
retInTry.f = 0;
function callsRet(n) { let o = 0; for (let i = 0; i < n; i++) if (retInTry(i) === 'odd') o++; return o + ':' + retInTry.f; }

let closed = 0;
function iterable(k) { return { [Symbol.iterator]() { let i = 0; return { next() { return { value: i, done: i++ >= k }; }, return() { closed++; return {}; } }; } }; }
function noReturn(k) { return { [Symbol.iterator]() { let i = 0; return { next() { return { value: i, done: i++ >= k }; } }; } }; }
function forOfExits(n) {
  let s = 0;
  for (let r = 0; r < n; r++) {
    for (const v of iterable(10)) { if (v === 3) break; s += v; }
    try { for (const v of iterable(10)) { if (v === 2) throw 0; s += v; } } catch (e) { s += 1000; }
    s += (() => { for (const v of iterable(10)) { if (v === 1) return 7; } return 0; })();
    for (const v of noReturn(3)) { if (v === 1) break; }
  }
  return s;
}
function* walk(t) { try { for (const k of t.kids) yield* walk(k); yield t.name; } finally { walk.done++; } }
walk.done = 0;
function walks(n) { const t = { name: 'a', kids: [{ name: 'b', kids: [] }, { name: 'c', kids: [{ name: 'd', kids: [] }] }] }; let out = ''; for (let i = 0; i < n; i++) out = [...walk(t)].join(''); return out; }
function earlyWalk(n) { const t = { name: 'a', kids: [{ name: 'b', kids: [] }, { name: 'c', kids: [] }] }; let c = 0; for (let i = 0; i < n; i++) for (const v of walk(t)) { c++; break; } return c; }

function level3(i) { if (i % 4 === 0) throw new Error('deep'); return 1; }
function level2(i) { try { return level3(i); } finally { level2.f++; } }
level2.f = 0;
function level1(n) { let c = 0; for (let i = 0; i < n; i++) { try { c += level2(i); } catch (e) { c += 100; } } return c; }

for (let r = 0; r < 3; r++) {
  check('catches ' + r, catches(3000), [1000 + 600 * 10 + 429 * 100, 2999 * 3000 / 2 - 3 * 999 * 1000 / 2, 3000].join());
  check('nested ' + r, nested(3000), 1500 * 11 + 1500 * 110);
  check('finally flow ' + r, finallyFlow(3000), finallyRef(3000));
  retInTry.f = 0; check('return in try ' + r, callsRet(3000), '1500:3000');
  closed = 0; check('for-of exits ' + r, forOfExits(1000), 1000 * (3 + 1 + 1000 + 7)); check('closed ' + r, closed, 3000);
  walk.done = 0; check('walks ' + r, walks(500), 'bdca'); check('walk finally ' + r, walk.done, 2000);
  walk.done = 0; check('early walk ' + r, earlyWalk(500), 500); check('early walk finally ' + r, walk.done, 1000);
  level2.f = 0; check('deep ' + r, level1(3000), 750 * 100 + 2250); check('deep finally ' + r, level2.f, 3000);
}
function finallyRef(n) {
  let s = 0;
  for (let i = 0; i < n; i++) {
    for (let j = 0; j < 4; j++) {
      if (j === 1) { s += 100; continue; }
      if (j === 2 && i % 2) { s += 100; break; }
      if (j === 3 && i % 5 === 0) { s += 100; break; }
      s += j + 100;
    }
  }
  return s;
}

console.log(bad === 0 ? 'ok' : 'FAILED ' + bad);
