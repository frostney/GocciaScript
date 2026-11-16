// private member reads and writes from compiled code through each site's cache: fields in inline and overflow
// slots, many shapes at one site, two evaluations of one class (different private names), private methods and
// accessors, brand-check failures, writes to methods, and stores of new objects across collections
let bad = 0;
function check(what, got, want) { if (!Object.is(got, want)) { bad++; console.log('FAIL', what, got, want); } }
function errName(f) { try { f(); return 'none'; } catch (e) { return e.constructor.name; } }

class P {
  #x; #y = 0;
  constructor(x, extra) { this.#x = x; for (let i = 0; i < extra; i++) this['p' + i] = i; }
  get x() { return this.#x; }
  bump() { this.#y++; return this.#y; }
  set(v) { this.#x = v; }
  static has(o) { return #x in o; }
  #m() { return 'm' + this.#x; }
  callM() { return this.#m(); }
  get #acc() { return this.#x * 2; }
  set #acc(v) { this.#x = v / 2; }
  useAcc(v) { this.#acc = v; return this.#acc; }
  writeMethod() { this.#m = 1; }
}

// many shapes (extra public properties push the private fields' slots around and into overflow)
const objs = [];
for (let i = 0; i < 24; i++) objs.push(new P(i, i));
let s = 0;
for (let r = 0; r < 2000; r++) for (const o of objs) { s += o.x; o.bump(); }
check('reads', s, 2000 * (23 * 24 / 2));
check('writes', objs[5].bump(), 2001);
for (const o of objs) o.set(o.x + 100);
check('set', objs[23].x, 123);
check('method', objs[3].callM(), 'm103');
check('accessor', objs[4].useAcc(50), 50);
check('accessor stored', objs[4].x, 25);
check('write to a method', errName(() => objs[0].writeMethod()), 'TypeError');

// a brand check failure after warm-up, and a primitive receiver
const plain = { x: 1 };
check('wrong object', errName(() => P.prototype.bump.call(plain)), 'TypeError');
check('primitive', errName(() => P.prototype.bump.call(3)), 'TypeError');
check('in', P.has(objs[0]) && !P.has(plain), true);

// two evaluations of one class: same shape layout, different private names
function makeClass() { return class { #v; constructor(v) { this.#v = v; } get v() { return this.#v; } static read(o) { return o.#v; } }; }
const A1 = makeClass(), A2 = makeClass();
const a1 = new A1(1), a2 = new A2(2);
let t = 0;
for (let i = 0; i < 3000; i++) t += A1.read(a1) + A2.read(a2);
check('two classes', t, 9000);
check('cross read throws', errName(() => A1.read(a2)), 'TypeError');
check('cross read throws 2', errName(() => A2.read(a1)), 'TypeError');

// new objects stored into private fields across collections
class Holder { #o = null; put(o) { this.#o = o; } get() { return this.#o; } }
const hs = [];
for (let i = 0; i < 64; i++) hs.push(new Holder());
for (let i = 0; i < 200000; i++) { const h = hs[i % 64]; h.put({ i, arr: [i, i] }); if (h.get().i !== i) { check('holder ' + i, h.get().i, i); break; } }
check('held after GC', hs.map(h => h.get().arr[1]).reduce((a, b) => a + b, 0), 64 * (200000 - 64) + 64 * 63 / 2);

console.log(bad === 0 ? 'ok' : 'FAILED ' + bad);
