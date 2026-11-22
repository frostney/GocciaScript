// A prototype lives as long as something reaches it, objects through their shapes included: a full collection
// retires the shapes of prototypes nothing reaches (G2.Core RetireDeadShapes) and drops their root shape entries.
// Instances of classes nothing else holds keep their methods; a constructor's cached root shape (CtorRootShape) is
// never used for a prototype that died, even when a new one lands at its address; a prototype whose instances are
// all gone is freed (a WeakRef to it clears); a proxy prototype stops disabling cached add transitions once it dies,
// and a setter on a prototype still runs after that
let bad = 0;
function check(what, got, want) { if (got !== want) { bad++; console.log('FAIL', what, String(got), String(want)); } }
const gc = () => G2Host.gc();

// classes made and dropped in a loop, every 50th instance kept
const kept = [];
const mk = (k) => {
  class Base { m() { return k; } get tag() { return 'b' + k; } }
  class Child extends Base { n() { return this.m() * 2; } }
  return new Child();
};
for (let k = 0; k < 2000; k++) {
  const o = mk(k);
  if (k % 50 === 0) kept.push([k, o]);
  if (k % 500 === 499) gc();
}
gc();
for (const [k, o] of kept) {
  check('m ' + k, o.m(), k);
  check('n ' + k, o.n(), 2 * k);
  check('tag ' + k, o.tag, 'b' + k);
  check('ctor ' + k, Object.getPrototypeOf(o).constructor.name, 'Child');
  check('instanceof ' + k, o instanceof Object.getPrototypeOf(o).constructor, true);
}

// one constructor, a new prototype object each round: its template caches the last prototype's root shape
function F() { this.x = 1; }
for (let k = 0; k < 300; k++) {
  F.prototype = { tag: k, pad: [k, k, k] };
  const o = new F();
  check('F proto ' + k, Object.getPrototypeOf(o), F.prototype);
  check('F tag ' + k, o.tag, k);
  if (k % 30 === 29) gc();
}

// a prototype set on one object, reachable only through that object
const lone = Object.setPrototypeOf({}, { hello() { return 'hi'; } });
gc(); gc();
check('setPrototypeOf survivor', lone.hello(), 'hi');

// null prototypes (the table's primitive entries)
const nulls = [];
for (let k = 0; k < 100; k++) { const o = Object.create(null); o.k = k; nulls.push(o); }
gc();
check('null proto', Object.getPrototypeOf(nulls[7]), null);
check('null proto key', nulls[99].k, 99);

// a proxy prototype disables cached add transitions while it lives
let proxyHits = 0;
let viaProxy = Object.create(new Proxy({}, { set(t, key, v, r) { proxyHits++; return Reflect.set(t, key, v, r); } }));
viaProxy.a = 1;
check('proxy set trap', proxyHits, 1);
viaProxy = null;
gc(); gc();
// after it died: a setter on an ordinary prototype still runs for every store, cached add transitions or not
let setterHits = 0;
const SP = { set v(x) { setterHits++; } };
for (let k = 0; k < 200; k++) {
  const o = { w: k };
  if (k === 100) Object.setPrototypeOf(o, SP);
  o.v = k;
  if (k === 100) check('setter own', Object.prototype.hasOwnProperty.call(o, 'v'), false);
}
check('setter hits', setterHits, 1);
const P2 = {};
const plain = [];
for (let k = 0; k < 100; k++) { const o = Object.create(P2); o.q = k; plain.push(o); }
Object.defineProperty(P2, 'r', { set(x) { setterHits++; }, configurable: true });
for (let k = 0; k < 100; k++) { const o = Object.create(P2); o.q = k; o.r = k; }
check('setter after define', setterHits, 101);

// a prototype whose instances are all gone is freed; each check runs as a job of its own (WeakRef's kept list)
let weak;
(() => {
  class Gone { m() { return 1; } }
  let inst = new Gone();
  weak = new WeakRef(Gone.prototype);
  check('gone alive', inst.m(), 1);
  inst = null;
})();
Promise.resolve().then(() => { gc(); gc(); }).then(() => {
  check('dead prototype freed', weak.deref(), undefined);
  console.log(bad === 0 ? 'ok' : 'bad ' + bad);
});
