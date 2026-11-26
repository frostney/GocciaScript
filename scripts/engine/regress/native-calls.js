// native functions called from compiled code through the call stub's own path: results, throws caught in the
// caller and further up, this given or not (undefined, a primitive, an object), no and many arguments, natives
// that call back into compiled code (and those calling natives again), natives that allocate across collections,
// natives called while a constructor runs (new.target), Function.prototype.call and apply, bound natives
let bad = 0;
function check(what, got, want) { if (!Object.is(got, want)) { bad++; console.log('FAIL', what, got, want); } }

function results(n) {
  let s = 0, a = [];
  for (let i = 0; i < n; i++) {
    a.push(i, i + 1);
    s += a.pop() + Math.max(i, 3, -1) + Math.min(i, 5) + 'abcdef'.charAt(i % 6).length + (Array.isArray(a) ? 1 : 0);
    s += Math.floor(i / 3) + 'x,y,z'.split(',').length + [1, 2, 3].indexOf(i % 4);
  }
  return s;
}
function throws(n) {
  let caught = 0;
  for (let i = 0; i < n; i++) {
    try { if (i % 3 === 0) JSON.parse('{'); else JSON.parse('[' + i + ']'); } catch (e) { if (e instanceof SyntaxError) caught++; }
  }
  return caught;
}
function throwsUp(i) { return i % 5 === 0 ? null.x : Number.parseInt('' + i, 10); }
function callThrowsUp(n) { let c = 0, s = 0; for (let i = 0; i < n; i++) { try { s += throwsUp(i); } catch (e) { c++; } } return c + ':' + s; }
function thisKinds(n) {
  let s = '';
  const toStr = Object.prototype.toString, hasOwn = Object.prototype.hasOwnProperty;
  for (let i = 0; i < n; i++) s = toStr.call(i & 1 ? [] : 5) + hasOwn.call({ a: 1 }, 'a') + String(i % 2);
  return s;
}
function manyArgs(n) { let s = 0; for (let i = 0; i < n; i++) s += Math.max(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, i); return s; }
function callbacks(n) {
  let s = 0;
  for (let i = 0; i < n; i++) s += [1, 2, 3].map(x => Math.abs(-x) * i).reduce((p, q) => Math.max(p, q), 0);
  return s;
}
function allocs(n) {
  const kept = [];
  for (let i = 0; i < n; i++) { const s = 'k' + i; const a = s.split(''); const o = Object.assign({}, { v: a.join('') }); if (i % 1000 === 0) kept.push(o); }
  return kept.map(o => o.v).join(',');
}
class Base { constructor(x) { this.x = Math.max(x, 0); this.t = typeof new.target; this.k = [x].concat([1]).length; } }
class Derived extends Base { constructor(x) { super(Math.abs(x)); this.y = Array.of(x, x).length; } }
function constructs(n) { let s = 0; for (let i = 0; i < n; i++) { const d = new Derived(-i); s += d.x + d.y + d.k; } return s; }
function reflected(n) {
  let s = 0;
  const bound = Math.max.bind(null, 7);
  for (let i = 0; i < n; i++) s += Reflect.apply(Math.min, null, [i, 4]) + Math.max.apply(null, [i, 2]) + Math.max.call(null, 1, i) + bound(i);
  return s;
}
function strictThis() { 'use strict'; return [1].map(function () { return this; })[0]; }

for (let r = 0; r < 3; r++) {
  check('results ' + r, results(3000), 10529991);
  check('throws ' + r, throws(3000), 1000);
  check('throws up ' + r, callThrowsUp(3000), '600:3600000');
  check('this kinds ' + r, thisKinds(3000), '[object Array]true1');
  check('many args ' + r, manyArgs(3000), 4498690);
  check('callbacks ' + r, callbacks(3000), 13495500);
  check('allocs ' + r, allocs(30000).length, 196);
  check('constructs ' + r, constructs(3000), 4510500);
  check('reflected ' + r, reflected(3000), 13507522);
  check('strict this ' + r, strictThis(), undefined);
}
check('native error message', (() => { try { JSON.parse('{'); } catch (e) { return e instanceof SyntaxError; } })(), true);

console.log(bad === 0 ? 'ok' : 'FAILED ' + bad);
