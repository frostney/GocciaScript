// getters reached through compiled code's property caches (the machine-code lookup handing a compiled getter to the
// getter stub): own, class-prototype and dictionary-prototype getters at polymorphic sites, getters not compiled
// yet, native getters, accessors without a getter, getters that throw, the result written over the receiver's
// register (o = o.g), getters redefined after the caches held them, and recursion through a getter to the stack
// bound
let bad = 0;
function check(what, got, want) { if (!Object.is(got, want)) { bad++; console.log('FAIL', what, got, want); } }

class Base { constructor(v) { this.v = v; } get g() { return this.v * 2; } }
class Mid extends Base { } class Leaf extends Mid { }
const ownG = { v: 5, get g() { return this.v + 1; } };
const dict = {}; for (let i = 0; i < 40; i++) dict['k' + i] = i; for (let i = 0; i < 38; i++) delete dict['k' + i];
Object.defineProperty(dict, 'g', { get() { return 100; }, configurable: true });
const viaDict = Object.create(dict);
const dataG = { g: 7 };
const noGetter = {}; Object.defineProperty(noGetter, 'g', { set(x) { }, configurable: true });

function readG(o) { return o.g; }
function sum(objs, n) { let s = 0; for (let r = 0; r < n; r++) for (let i = 0; i < objs.length; i++) { const v = readG(objs[i]); s += v === undefined ? 1000 : v; } return s; }
function slow(objs, n) { let s = 0; for (let r = 0; r < n; r++) for (let i = 0; i < objs.length; i++) { const v = Reflect.get(objs[i], 'g'); s += v === undefined ? 1000 : v; } return s; }
function overwrite(o) { o = o.g; return o; }

const objs = [new Base(1), new Mid(2), new Leaf(3), ownG, viaDict, dataG, noGetter];
for (let round = 0; round < 3; round++) {
  check('getters ' + round, sum(objs, 2000), slow(objs, 2000));
  let w = 0, ws = 0;
  for (let i = 0; i < 3000; i++) { w += overwrite(objs[i % 5]); ws += Reflect.get(objs[i % 5], 'g'); }
  check('overwrite ' + round, w, ws);
  // redefined after caching
  Object.defineProperty(dict, 'g', { get() { return 200 + round; }, configurable: true });
  Object.defineProperty(Base.prototype, 'g', { get() { return this.v * 3; }, configurable: true });
}

// a getter compiled only after the site is hot, then native getters (Map size, typed array length) at the site
let late = 0;
const lateObj = {}; Object.defineProperty(lateObj, 'g', { get() { late++; return 1; } });
check('late getter', sum([lateObj, new Base(1)], 3000), 3000 * (1 + 3));
check('late getter calls', late, 3000);
function size(o) { return o.size; }
const m = new Map([[1, 1], [2, 2]]); const st = new Set([1]);
const sized = [m, st, { size: 9 }, { get size() { return 4; } }];
let ss = 0; for (let i = 0; i < 4000; i++) ss += size(sized[i & 3]);
check('native getters', ss, 1000 * (2 + 1 + 9 + 4));

// a getter that throws, caught in the caller
const thrower = { get g() { throw new RangeError('g'); } };
let caught = 0; for (let i = 0; i < 3000; i++) { try { readG(i % 3 ? new Base(1) : thrower); } catch (e) { if (e instanceof RangeError) caught++; } }
check('throwing getter', caught, 1000);

// recursion through a getter, deep enough to reach the stack bound
const deep = { n: 0, get down() { this.n++; return this.down; } };
let re = 'none'; try { deep.down; } catch (e) { re = e instanceof RangeError ? 'RangeError' : String(e); }
check('stack bound', re, 'RangeError');
check('after bound', readG(new Base(4)), 12);

console.log(bad === 0 ? 'ok' : 'FAILED ' + bad);
