// property reads at sites with more receiver shapes than their inline cache holds, and reads of keys absent
// from the whole chain, see later changes: a key added to a prototype, to the receiver, to a dictionary object
// on the way (in place) or at the end, an accessor put on a prototype, an array's length against a plain
// object of the same shape
let bad = 0;
function check(what, got, want) { if (!Object.is(got, want)) { bad++; console.log('FAIL', what, got, want); } }
function readK(o) { return o.k; }
function readLen(o) { return o.length; }
function warm(f, objs, n) { let s = 0; for (let i = 0; i < n; i++) { const v = f(objs[i % objs.length]); if (typeof v === 'number') s += v; } return s; }
// a megamorphic site: twelve shapes, k own at different slots or inherited
const shapes = [];
for (let i = 0; i < 12; i++) {
  const o = {};
  for (let j = 0; j < i; j++) o['p' + j] = j;
  if (i % 3 === 0) o.k = i; else if (i % 3 === 1) Object.setPrototypeOf(o, { k: i * 10 });
  shapes.push(o);
}
for (let r = 0; r < 50; r++) warm(readK, shapes, 120);
for (let i = 0; i < 12; i++) check('mega ' + i, readK(shapes[i]), i % 3 === 0 ? i : i % 3 === 1 ? i * 10 : undefined);
// absent keys: then added to the prototype, then to the receiver
class P {}
const ps = [];
for (let i = 0; i < 8; i++) { const o = new P(); for (let j = 0; j <= i; j++) o['q' + j] = j; ps.push(o); }
for (let r = 0; r < 50; r++) warm(readK, ps, 80);
check('absent', readK(ps[3]), undefined);
P.prototype.k = 'proto';
check('added to the prototype', readK(ps[3]), 'proto');
ps[3].k = 'own';
check('added to the receiver', readK(ps[3]), 'own');
check('others still inherit', readK(ps[4]), 'proto');
delete P.prototype.k;
check('deleted from the prototype', readK(ps[4]), undefined);
Object.defineProperty(P.prototype, 'k', { get() { return 'getter'; }, configurable: true });
check('accessor on the prototype', readK(ps[5]), 'getter');
delete P.prototype.k;
// a dictionary prototype in the middle (more than 64 keys) gains the key in place
const big = {};
for (let j = 0; j < 100; j++) big['b' + j] = j;
const ds = [];
for (let i = 0; i < 8; i++) { const o = Object.create(big); for (let j = 0; j <= i; j++) o['d' + j] = j; ds.push(o); }
for (let r = 0; r < 50; r++) warm(readK, ds, 80);
check('absent past a dictionary', readK(ds[2]), undefined);
big.k = 'dict';
check('added to a dictionary on the way', readK(ds[2]), 'dict');
delete big.k;
check('deleted from the dictionary', readK(ds[2]), undefined);
// dictionary receivers gain the key in place
const drs = [];
for (let i = 0; i < 6; i++) { const o = {}; for (let j = 0; j < 70 + i; j++) o['x' + j] = j; drs.push(o); }
for (let r = 0; r < 50; r++) warm(readK, drs, 60);
check('absent on a dictionary', readK(drs[1]), undefined);
drs[1].k = 'dk';
check('added to a dictionary receiver', readK(drs[1]), 'dk');
// a chain ending in a dictionary with a null prototype
const end = Object.create(null);
for (let j = 0; j < 80; j++) end['e' + j] = j;
const es = [];
for (let i = 0; i < 6; i++) { const o = Object.create(end); for (let j = 0; j <= i; j++) o['f' + j] = j; es.push(o); }
for (let r = 0; r < 50; r++) warm(readK, es, 60);
check('absent at a dictionary end', readK(es[1]), undefined);
end.k = 'end';
check('added to the dictionary end', readK(es[1]), 'end');
// length: arrays (computed), plain objects of many shapes (absent or own)
const ls = [[1, 2, 3], { a: 1 }, { length: 7 }, [1], { b: 1, c: 2 }, 'str', { d: 1 }, { e: 1, f: 1 }, new Array(5)];
for (let r = 0; r < 50; r++) warm(readLen, ls, 90);
check('array length', readLen(ls[0]), 3);
check('plain without length', readLen(ls[1]), undefined);
check('own length', readLen(ls[2]), 7);
check('string length', readLen(ls[5]), 3);
ls[0].push(4);
check('array length after push', readLen(ls[0]), 4);
Object.prototype.length = 'op';
check('length on Object.prototype', readLen(ls[1]), 'op');
check('array length still own', readLen(ls[8]), 5);
delete Object.prototype.length;
check('length gone again', readLen(ls[4]), undefined);
console.log(bad === 0 ? 'ok' : 'FAIL ' + bad);
