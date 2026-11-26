// stores into an array's holes below its length (new Array(n), a longer length) see setters and read-only
// elements put on Array.prototype or Object.prototype later, and a frozen array; closures that capture in any
// register order share their variables
let bad = 0;
function check(what, got, want) { if (!Object.is(got, want)) { bad++; console.log('FAIL', what, got, want); } }
function fill(a, n) { for (let i = 0; i < n; i++) a[i] = i * 2; return a; }
function fillStrict(a, n) { 'use strict'; for (let i = 0; i < n; i++) a[i] = i * 2; return a; }
for (let r = 0; r < 300; r++) { fill(new Array(16), 16); const b = [1]; b.length = 16; fill(b, 16); }
check('holes filled', fill(new Array(8), 8).join(','), '0,2,4,6,8,10,12,14');
let seen = 0;
Object.defineProperty(Array.prototype, 5, { set(v) { seen = v; }, get() { return 'g'; }, configurable: true });
const a1 = fill(new Array(8), 8);
check('setter on Array.prototype called', seen, 10);
check('no own element past the setter', Object.prototype.hasOwnProperty.call(a1, 5), false);
delete Array.prototype[5];
Object.defineProperty(Object.prototype, 6, { set(v) { seen = -v; }, configurable: true });
fill(new Array(8), 8);
check('setter on Object.prototype called', seen, -12);
delete Object.prototype[6];
Object.defineProperty(Array.prototype, 2, { value: 'ro', writable: false, configurable: true });
const a2 = fill(new Array(4), 4);
check('read-only prototype element: no own element', Object.prototype.hasOwnProperty.call(a2, 2), false);
let threw = false;
try { fillStrict(new Array(4), 4); } catch (e) { threw = e instanceof TypeError; }
check('read-only prototype element: strict throws', threw, true);
delete Array.prototype[2];
const fr = Object.freeze(new Array(4));
fill(fr, 4);
check('frozen array keeps its holes', fr.length === 4 && !(0 in fr), true);
check('plain store again', fill(new Array(4), 4).join(','), '0,2,4,6');
// upvalues: captures made in descending register order, shared between closures, closed on return
function make(n) {
  let a = n, b = n + 1, c = n + 2;
  const fs = [() => c + b + a, () => { a += 10; return a; }, () => { c += 100; return c; }, () => b * 2];
  return fs;
}
let s = 0;
for (let r = 0; r < 2000; r++) { const fs = make(r); fs[1](); fs[2](); s += fs[0]() + fs[3](); }
let want = 0;
for (let r = 0; r < 2000; r++) want += (r + 2 + 100) + (r + 1) + (r + 10) + (r + 1) * 2;
check('shared captures', s, want);
function nest(n) { let x = n; function inner() { let y = x + 1; return () => { x += y; return x; }; } const g = inner(); g(); return [g(), x]; }
check('nested captures', nest(3).join(','), '11,11');
console.log(bad === 0 ? 'ok' : 'FAIL ' + bad);
