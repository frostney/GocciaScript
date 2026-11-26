// a generator iterated as itself with the intrinsic next only while the protocol is intact: next or @@iterator
// overridden on a generator function's prototype, %GeneratorPrototype%.next replaced (and restored),
// %IteratorPrototype%[@@iterator] replaced, own properties on a generator object, a prototype in dictionary mode;
// through spread, destructuring, Array.from, for-of and yield*
let bad = 0;
function check(what, got, want) { if (!Object.is(got, want)) { bad++; console.log('FAIL', what, got, want); } }

function* g(n) { for (let i = 0; i < n; i++) yield i; }
// compiled consumers (warmed up below), so the checks run through compiled code's iteration
function forOfJoin(it) { let s = ''; for (const v of it) s += v; return s; }
function spreadJoin(it) { return [...it].join(''); }
function firstTwo(it) { const [a, b] = it; return a + ':' + b; }
for (let w = 0; w < 3000; w++) { forOfJoin(g(2)); spreadJoin(g(2)); firstTwo(g(2)); }
function* outer(n) { yield* g(n); }
function all(n) {
  const a = [...g(n)].join('');
  const [x, y] = g(n);
  const b = Array.from(g(n)).join('');
  let c = ''; for (const v of g(n)) c += v;
  const d = [...outer(n)].join('');
  return [a, x + ':' + y, b, c, d].join('|');
}
const plain = '012|0:1|012|012|012';
for (let r = 0; r < 300; r++) if (all(3) !== plain) { check('plain ' + r, all(3), plain); break; }

// next overridden on the generator function's prototype
g.prototype.next = function () { const d = (this.k = (this.k || 0) + 1) > 2; return { value: d ? undefined : 'p', done: d }; };
check('own next on prototype', [...g(3)].join(''), 'pp');
check('own next on prototype, compiled for-of', forOfJoin(g(3)), 'pp');
delete g.prototype.next;
check('restored', all(3), plain);

// @@iterator on the generator function's prototype
g.prototype[Symbol.iterator] = function () { return [7, 8][Symbol.iterator](); };
check('iterator on prototype', [...g(3)].join(''), '78');
check('iterator on prototype, compiled', forOfJoin(g(3)) + spreadJoin(g(3)), '7878');
delete g.prototype[Symbol.iterator];
check('restored again', all(3), plain);

// %GeneratorPrototype%.next replaced, then restored
const GenProto = Object.getPrototypeOf(g.prototype);
const realNext = GenProto.next;
let calls = 0;
GenProto.next = function (v) { calls++; return realNext.call(this, v); };
check('wrapped next', all(3), plain);
check('wrapped next calls', calls > 0, true);
calls = 0; check('wrapped next, compiled', forOfJoin(g(3)), '012'); check('wrapped next calls, compiled', calls, 4);
GenProto.next = realNext; calls = 0;
check('next restored', all(3), plain);

// %IteratorPrototype%[@@iterator] replaced
const IterProto = Object.getPrototypeOf(GenProto);
const realIter = IterProto[Symbol.iterator];
IterProto[Symbol.iterator] = function () { return [9][Symbol.iterator](); };
check('iterator replaced', [...g(3)].join(''), '9');
check('iterator replaced, compiled', forOfJoin(g(3)) + firstTwo(g(3)), '99:undefined');
IterProto[Symbol.iterator] = realIter;
check('iterator restored', all(3), plain);

// own properties on a generator object, and a prototype in dictionary mode
const gen = g(3); gen.next = () => ({ value: 'own', done: true });
check('own next on generator', [...gen].join(''), '');
const gen3 = g(3); gen3.next = () => ({ value: 'own', done: true }); check('own next on generator, compiled', forOfJoin(gen3), '');
const gen2 = g(2); gen2[Symbol.iterator] = () => [5][Symbol.iterator]();
check('own iterator on generator', [...gen2].join(''), '5');
function* h(n) { for (let i = 0; i < n; i++) yield i * 2; }
for (let i = 0; i < 40; i++) h.prototype['x' + i] = i; for (let i = 0; i < 40; i++) delete h.prototype['x' + i];
check('dictionary prototype', [...h(3)].join(''), '024');
h.prototype.next = function () { return { done: true }; };
check('dictionary prototype next', [...h(3)].join(''), '');
check('dictionary prototype next, compiled', forOfJoin(h(3)), '');

// closing: a break (the generator's finally runs), a throw (the throw wins over its return's), return
// overridden on a generator function's prototype and on %GeneratorPrototype%, destructuring that stops early
let fin = 0;
function* f(n) { try { for (let i = 0; i < n; i++) yield i; } finally { fin++; } }
function breakEarly(it) { for (const v of it) if (v === 1) break; }
function throwInside(it) { try { for (const v of it) if (v === 1) throw new RangeError('in'); } catch (e) { return e.message; } return 'none'; }
function firstOnly(it) { const [a] = it; return a; }
for (let w = 0; w < 3000; w++) { breakEarly(f(3)); throwInside(f(3)); firstOnly(f(3)); }
fin = 0;
for (let i = 0; i < 100; i++) { breakEarly(f(3)); firstOnly(f(3)); }
check('finally on close', fin, 200);
fin = 0; check('throw wins', throwInside(f(3)), 'in'); check('finally on throw', fin, 1);
function* thrower() { try { yield 1; yield 2; } finally { throw new TypeError('from finally'); } }
check('finally throw on break', (() => { try { breakEarly(thrower()); return 'none'; } catch (e) { return e.message; } })(), 'from finally');
check('finally throw loses to body throw', throwInside(thrower()), 'in');
let ownRet = 0;
f.prototype.return = function () { ownRet++; return { done: true }; };
fin = 0; breakEarly(f(3)); check('own return on prototype', ownRet + ':' + fin, '1:0');
delete f.prototype.return;
const realReturn = GenProto.return;
let wrapped = 0;
GenProto.return = function (v) { wrapped++; return realReturn.call(this, v); };
fin = 0; breakEarly(f(3)); firstOnly(f(3)); check('wrapped return', wrapped + ':' + fin, '2:2');
GenProto.return = realReturn;
fin = 0; breakEarly(f(3)); check('return restored', fin, 1);

console.log(bad === 0 ? 'ok' : 'FAILED ' + bad);
