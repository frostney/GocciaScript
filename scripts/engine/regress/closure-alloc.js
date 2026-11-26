// closures made in one nursery bump (upvalue array, function object, new upvalue cells): captures of the frame's
// registers shared between closures and fresh per loop iteration, captures of the parent's upvalues, three levels
// of nesting, more than 16 captures (the general path), arrows, name and length, generators, and many closures
// kept across collections
let bad = 0;
function check(what, got, want) { if (!Object.is(got, want)) { bad++; console.log('FAIL', what, got, want); } }

function counterPair() { let n = 0; return [() => ++n, () => n]; }
function perIteration(k) { const fs = []; for (let i = 0; i < k; i++) fs.push(() => i); return fs; }
function nested(a) { let b = a * 2; return function mid(c) { let d = c + b; return (e) => a + b + c + d + e; }; }
function many(v) {
  let a0 = v, a1 = v + 1, a2 = v + 2, a3 = v + 3, a4 = v + 4, a5 = v + 5, a6 = v + 6, a7 = v + 7, a8 = v + 8,
    a9 = v + 9, a10 = v + 10, a11 = v + 11, a12 = v + 12, a13 = v + 13, a14 = v + 14, a15 = v + 15, a16 = v + 16;
  return () => a0 + a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 + a14 + a15 + a16;
}
function shareTwo() { let x = 1, y = 2; const f = () => x + y, g = () => { x = 10; y = 20; }; return [f, g]; }
function* gen(k) { let i = 0; while (i < k) yield i++; }
function makeGen(k) { return () => gen(k); }

let ok = true;
for (let r = 0; r < 20000 && ok; r++) {
  const [inc, get] = counterPair();
  inc(); inc();
  if (get() !== 2) { check('counter ' + r, get(), 2); ok = false; }
  const fs = perIteration(5);
  if (fs.map(f => f()).join() !== '0,1,2,3,4') { check('per iteration ' + r, fs.map(f => f()).join(), '0,1,2,3,4'); ok = false; }
  const f3 = nested(r)(1);
  if (f3(1) !== r + 2 * r + 1 + (1 + 2 * r) + 1) { check('nested ' + r, f3(1), 0); ok = false; }
  if (many(r)() !== 17 * r + 136) { check('many ' + r, many(r)(), 17 * r + 136); ok = false; }
  const [f, g] = shareTwo();
  g();
  if (f() !== 30) { check('shared ' + r, f(), 30); ok = false; }
}
check('arrow this', (function () { return (() => this)(); }).call(globalThis), globalThis);
check('name', nested(1).name, 'mid');
check('length', nested(1)(2).length, 1);
check('generator closure', [...makeGen(4)()].join(), '0,1,2,3');
check('arrow has no prototype', 'prototype' in perIteration(1)[0], false);
check('function has prototype', typeof nested(1).prototype, 'object');

// many closures kept across collections
const kept = [];
for (let i = 0; i < 300000; i++) { const [inc, get] = counterPair(); inc(); if (i % 1000 === 0) kept.push(get); }
check('kept closures', kept.reduce((s, get) => s + get(), 0), 300);

console.log(bad === 0 ? 'ok' : 'FAILED ' + bad);
