// generator and async functions called from compiled code (the call made there, not left to the interpreter):
// arguments, defaults and destructured parameters, methods with this and super, sloppy this, arguments objects,
// yield* delegation and recursive generators walked by next(), for-of and spread, async functions and async
// generators (their results and the order of their effects), and errors thrown while the parameters are bound
let bad = 0;
function check(what, got, want) { if (!Object.is(got, want)) { bad++; console.log('FAIL', what, got, want); } }

function* range(a, b = a + 3, { step } = { step: 1 }) { for (let i = a; i < b; i += step) yield i; }
function* withArgs() { yield arguments.length; yield* arguments; }
function* sloppyThis() { yield this === globalThis; }
class Node2 {
  constructor(name, kids) { this.name = name; this.kids = kids; }
  *walk() { yield this.name; for (const k of this.kids) yield* k.walk(); }
}
class Leaf2 extends Node2 { constructor(name) { super(name, []); } *walk() { yield* super.walk(); yield '!'; } }
function* thrower(x = (() => { throw new RangeError('default'); })()) { yield x; }

function sumRange(n) { let s = 0; for (let i = 0; i < n; i++) for (const v of range(i)) s += v; return s; }
function nexts(n) { let s = 0; for (let i = 0; i < n; i++) { const g = range(i, i + 2); s += g.next().value + g.next().value; if (!g.next().done) s = -1e9; } return s; }
function spreads(n) { let s = 0; for (let i = 0; i < n; i++) s += [...range(0, 4, { step: 2 })].length + [...withArgs(i, 1, 2)][0]; return s; }
function thisChecks(n) { let c = 0; for (let i = 0; i < n; i++) if (sloppyThis().next().value) c++; return c; }
function trees(n) {
  const t = new Node2('a', [new Node2('b', [new Leaf2('c')]), new Leaf2('d')]);
  let out = '';
  for (let i = 0; i < n; i++) out = [...t.walk()].join('');
  return out;
}
function throws(n) { let c = 0; for (let i = 0; i < n; i++) { try { thrower(); } catch (e) { if (e instanceof RangeError) c++; } } return c; }

for (let r = 0; r < 3; r++) {
  check('for-of ' + r, sumRange(2000), 2000 * 3 * 1999 / 2 + 2000 * 3);
  check('next ' + r, nexts(2000), 2 * 1999 * 2000 / 2 + 2000);
  check('spread ' + r, spreads(2000), 2000 * 2 + 2000 * 3);
  check('sloppy this ' + r, thisChecks(2000), 2000);
  check('trees ' + r, trees(500), 'abc!d!');
  check('default throws ' + r, throws(2000), 2000);
}

// async functions and async generators called from a compiled loop: the effects' order and the results
const log = [];
async function af(i) { log.push('start ' + i); await null; log.push('end ' + i); return i * 2; }
async function* ag(n) { for (let i = 0; i < n; i++) { await null; yield i; } }
function startMany(n) { const ps = []; for (let i = 0; i < n; i++) ps.push(af(i)); return ps; }
const ps = startMany(1500);
check('async sync part', log.length, 1500);
check('async first', log[0], 'start 0');
Promise.all(ps).then(async vals => {
  check('async results', vals.reduce((a, b) => a + b, 0), 1499 * 1500);
  check('async order', log[1500], 'end 0');
  let s = 0;
  for (let k = 0; k < 300; k++) for await (const v of ag(3)) s += v;
  check('async generator', s, 300 * 3);
  console.log(bad === 0 ? 'ok' : 'FAILED ' + bad);
});
