// compiled code's calls through the callee's own entry (JitEnter): this for strict, sloppy, arrow and this-free
// callees (none, undefined, null, an object, a primitive), fewer and more arguments than parameters, callees not
// compiled yet, class constructors called without new, generators, async functions, bound functions and natives at
// the same sites, a callee that stops part way (its entry counted down) and one that throws, recursion deep enough
// to reach the stack bound, and calls from inlined callees in optimized code whose callee runs part way in the
// interpreter (the call must not run twice)
let bad = 0;
function check(what, got, want) { if (!Object.is(got, want)) { bad++; console.log('FAIL', what, got, want); } }

function sloppyThis() { return this; }
function strictThis() { 'use strict'; return this; }
const arrowThis = () => typeof this;
function thisFree(a, b) { return a + b; }
function sloppyType() { return typeof this; }
String.prototype.selfType = sloppyType;
function first(a, b, c) { return [a, b, c].map(x => x === undefined ? 'u' : x).join(''); }
class K { constructor(x) { this.x = x; } }
function* gen(n) { yield n; yield n + 1; }
async function asy(n) { return n; }
const bound = thisFree.bind(null, 10);

function sites(i, f) { return f(i, 1); }
function thisSites(n) {
  let g = 0, u = 0, o = 0, p = 0, s = 0;
  const obj = { sloppyThis, strictThis };
  for (let i = 0; i < n; i++) {
    if (sloppyThis() === globalThis) g++;
    if (strictThis() === undefined) u++;
    if (obj.sloppyThis() === obj && obj.strictThis() === obj) o++;
    if ('ab'.selfType() === 'object') p++;
    if (arrowThis() === typeof globalThis && thisFree(1, 2) === 3) s++;
  }
  return [g, u, o, p, s].join();
}
function argCounts(n) {
  let r = '';
  for (let i = 0; i < n; i++) r = first() + first(1) + first(1, 2) + first(1, 2, 3) + first(1, 2, 3, 4, 5);
  return r;
}
function mixed(n) {
  const fs = [thisFree, bound, Math.max, (a, b) => a * b, function (a) { return arguments.length; }];
  let s = 0;
  for (let i = 0; i < n; i++) s += sites(i % 7, fs[i % fs.length]);
  return s;
}
function notCallable(n) {
  let c = 0;
  for (let i = 0; i < n; i++) {
    try { K(i); } catch (e) { if (e instanceof TypeError) c++; }
    try { (i & 1 ? null : 5)(); } catch (e) { if (e instanceof TypeError) c++; }
  }
  return c;
}
function genAndAsync(n) { let s = 0; for (let i = 0; i < n; i++) { s += [...gen(i)].length; if (!(asy(i) instanceof Promise)) s = -1e9; } return s; }
function thrower(i) { if (i % 3 === 0) throw new RangeError('r' + i); return i; }
function throws(n) { let c = 0, s = 0; for (let i = 0; i < n; i++) { try { s += thrower(i); } catch (e) { c++; } } return c + ':' + s; }
function depth(n) { return n === 0 ? 0 : depth(n - 1) + 1; }
function tooDeep() { try { depth(1e7); return 'no error'; } catch (e) { return e instanceof RangeError; } }

// a callee inlined into a loop whose own recursive call runs part way in the interpreter (each entry counted down)
let effects = 0;
const o6 = { r(k, d) { if (d === 0) { if (k % 1000 === 4) effects++; return 1; } return o6.r(k, d - 1) + 1; } };
function loop6(n) { let t = 0; for (let i = 0; i < n; i++) t += o6.r(i, 2); return t; }

for (let r = 0; r < 3; r++) {
  check('this ' + r, thisSites(3000), '3000,3000,3000,3000,3000');
  check('arguments ' + r, argCounts(3000), 'uuu1uu12u123123');
  check('mixed ' + r, mixed(3500), 17600);
  check('not callable ' + r, notCallable(2000), 4000);
  check('generators and async ' + r, genAndAsync(2000), 4000);
  check('throws ' + r, throws(3000), '1000:3000000');
  effects = 0;
  check('inlined recursion ' + r, loop6(5000), 15000);
  check('inlined recursion effects ' + r, effects, 5);
}
check('stack bound', tooDeep(), true);
check('depth after', depth(1000), 1000);

console.log(bad === 0 ? 'ok' : 'FAILED ' + bad);
