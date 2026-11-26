// getters that compiled code calls straight through the call stub: own and inherited class and literal getters,
// a getter's this and result, throws, recursion past the stack, getters that return methods (o.m()), getters
// replaced by data properties or other getters, sloppy getters, native getters, and collections inside getters
let bad = 0;
function check(what, got, want) { if (!Object.is(got, want)) { bad++; console.log('FAIL', what, got, want); } }

class Box {
  constructor(v) { this.v = v; }
  get value() { return this.v; }
  get self() { return this; }
  get twice() { return this.value * 2; }
  get method() { return function () { return this.v + 100; }; }
}
class SubBox extends Box { get value() { return this.v + 1; } }
const lit = { k: 3, get g() { return this.k * 10; } };
const inherits = Object.create(lit);
inherits.k = 4;

function readValue(o) { return o.value; }
function readTwice(o) { return o.twice; }
function readSelf(o) { return o.self; }
function callMethod(o) { return o.method(); }
function readG(o) { return o.g; }

let s = 0;
for (let i = 0; i < 20000; i++) {
  const b = i % 3 === 0 ? new SubBox(i) : new Box(i);
  const want = i % 3 === 0 ? i + 1 : i;
  if (readValue(b) !== want || readTwice(b) !== want * 2 || readSelf(b) !== b || callMethod(b) !== i + 100) {
    check('box ' + i, [readValue(b), readTwice(b), readSelf(b) === b, callMethod(b)].join(), [want, want * 2, true, i + 100].join());
    break;
  }
  s += readG(i % 2 ? lit : inherits);
}
check('literal getters', s, 10000 * 30 + 10000 * 40);

// a getter that throws, sometimes
class Thrower { constructor(n) { this.n = n; } get x() { if (this.n % 7 === 0) throw new RangeError('n ' + this.n); return this.n; } }
function readX(o) { return o.x; }
let caught = 0, sum = 0;
for (let i = 0; i < 7000; i++) { try { sum += readX(new Thrower(i)); } catch (e) { if (e instanceof RangeError && e.message === 'n ' + i) caught++; } }
check('throws caught', caught, 1000);
check('throws sum', sum, 7000 * 6999 / 2 - 7 * (999 * 1000 / 2));

// recursion through a getter past the stack: a RangeError, then things still work
const deep = { get d() { return this.d + 1; } };
function readD(o) { return o.d; }
let deepErr = 'none';
for (let i = 0; i < 3; i++) { try { readD(deep); } catch (e) { deepErr = e.constructor.name; } }
check('deep recursion', deepErr, 'RangeError');
check('after deep recursion', readValue(new Box(5)), 5);

// a getter replaced by a data property, by another getter, and deleted
class Swap { get p() { return 'getter'; } }
const sw = new Swap();
function readP(o) { return o.p; }
for (let i = 0; i < 3000; i++) readP(sw);
check('getter', readP(sw), 'getter');
Object.defineProperty(Swap.prototype, 'p', { get() { return 'other'; }, configurable: true });
check('other getter', readP(sw), 'other');
Object.defineProperty(Swap.prototype, 'p', { value: 'data', configurable: true });
check('data', readP(sw), 'data');
delete Swap.prototype.p;
check('deleted', readP(sw), undefined);

// sloppy and strict getters on objects, this as the receiver (not the holder)
const sloppyProto = { get who() { return this; } };
const kid = Object.create(sloppyProto);
function readWho(o) { return o.who; }
for (let i = 0; i < 3000; i++) if (readWho(kid) !== kid) { check('sloppy this', 'holder', 'receiver'); break; }

// a native getter (an accessor of a built-in) and the legacy caller accessor through compiled code
function readSize(m) { return m.size; }
const m = new Map([[1, 1], [2, 2]]);
let sz = 0;
for (let i = 0; i < 3000; i++) sz += readSize(m);
check('native getter', sz, 6000);
function sloppyFn() { return readCaller(sloppyFn); }
function readCaller(f) { return f.caller; }
let callerOk = true;
for (let i = 0; i < 3000; i++) if (sloppyFn() !== null && typeof sloppyFn() !== 'function') { callerOk = false; break; }
check('legacy caller', callerOk, true);

// collections inside getters; the getter's result survives
class Alloc { constructor(i) { this.i = i; } get fresh() { const a = []; for (let k = 0; k < 20; k++) a.push({ k, i: this.i }); return a; } }
function readFresh(o) { return o.fresh; }
const kept = [];
for (let i = 0; i < 20000; i++) { const r = readFresh(new Alloc(i)); if (i % 1000 === 0) kept.push(r); if (r[19].i !== i) { check('alloc ' + i, r[19].i, i); break; } }
check('kept results', kept.map(a => a[5].i).join(), Array.from({ length: 20 }, (_, i) => i * 1000).join());

// getters reached from many frames deep, and from within a getter
class Chain { constructor(n) { this.n = n; this.next = n > 0 ? new Chain(n - 1) : null; } get total() { return this.n + (this.next ? this.next.total : 0); } }
const ch = new Chain(50);
function readTotal(o) { return o.total; }
let tot = 0;
for (let i = 0; i < 300; i++) tot = readTotal(ch);
check('nested getters', tot, 1275);

console.log(bad === 0 ? 'ok' : 'FAILED ' + bad);
