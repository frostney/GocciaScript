// compiled code's cold property gets (its first cached shape missed): own data properties of the other shapes a
// site has seen, in inline and overflow slots, data properties one to three prototypes up, prototypes in
// dictionary mode changed in place after the site cached them (their versions), accessors on either side, and
// receivers whose shapes change after the site cached them
let bad = 0;
function check(what, got, want) { if (!Object.is(got, want)) { bad++; console.log('FAIL', what, got, want); } }

function getV(o) { return o.v; }
function sumV(objs, n) { let s = 0; for (let r = 0; r < n; r++) for (let i = 0; i < objs.length; i++) s += getV(objs[i]); return s; }

// four own shapes, one with the property past twenty others (an overflow slot)
const wide = {}; for (let i = 0; i < 24; i++) wide['p' + i] = i; wide.v = 4;
const own = [{ v: 1 }, { a: 0, v: 2 }, { a: 0, b: 0, v: 3 }, wide];
check('own poly', sumV(own, 3000), 3000 * 10);

// prototypes: one, two and three up, and the first shape's own
class A { } A.prototype.v = 10;
class B extends A { } class C extends B { }
const proto = [{ v: 1 }, new A(), new B(), new C()];
check('proto depths', sumV(proto, 3000), 3000 * 31);

// a dictionary prototype (many deletes) changed in place after the site cached it
const dp = {}; for (let i = 0; i < 40; i++) dp['x' + i] = i; for (let i = 0; i < 39; i++) delete dp['x' + i];
dp.v = 7;
const viaDict = [{ v: 1 }, Object.create(dp), { q: 1, v: 2 }];
check('dict proto', sumV(viaDict, 2000), 2000 * 10);
dp.v = 8;
check('dict proto changed', sumV(viaDict, 10), 10 * 11);
delete dp.v; Object.prototype.v = 100;
check('dict proto deleted', sumV(viaDict, 10), 10 * 103);
delete Object.prototype.v;

// accessors on the receiver and on a prototype among data properties
let calls = 0;
const acc = { get v() { calls++; return 5; } };
const accProto = Object.create({ get v() { calls++; return 6; } });
const mixed = [{ v: 1 }, acc, { b: 1, v: 2 }, accProto];
check('accessors', sumV(mixed, 2000), 2000 * 14);
check('accessor calls', calls, 4000);

// receivers changing shape after caching: properties added, deleted, overwritten with accessors
const morph = [{ v: 1 }, { a: 1, v: 2 }, { b: 1, v: 3 }];
check('morph before', sumV(morph, 2000), 2000 * 6);
morph[1].z = 9; delete morph[2].b;
Object.defineProperty(morph[0], 'v', { get() { return 50; } });
check('morph after', sumV(morph, 2000), 2000 * 55);
check('missing', sumV([{ v: 1 }, { a: 1 }, { b: 2, v: 3 }], 10), NaN);

console.log(bad === 0 ? 'ok' : 'FAILED ' + bad);
