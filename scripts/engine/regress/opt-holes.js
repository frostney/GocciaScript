// optimized loops that read and store holes (new Array(n)): undefined from a hole while no index-keyed property
// on the way can answer it, the prototype's answer once Array.prototype or Object.prototype gains an element, an
// index accessor or a proxy, and stores into holes of frozen, sealed and non-extensible arrays or past an index
// setter on a prototype
let bad = 0;
function check(what, got, want) { if (!Object.is(got, want)) { bad++; console.log('FAIL', what, got, want); } }

function countHoles(a) { let n = 0; for (let i = 0; i < a.length; i++) { if (a[i] === undefined) n++; } return n; }
function fill(a, step) { for (let i = 0; i < a.length; i += step) a[i] = i; return a; }
function sumDefined(a) { let s = 0; for (let i = 0; i < a.length; i++) { const v = a[i]; if (v !== undefined) s += v; } return s; }

// warm: holes read and stored in optimized loops
for (let r = 0; r < 200; r++) {
  const a = new Array(64);
  if (countHoles(a) !== 64) { check('holes ' + r, countHoles(a), 64); break; }
  fill(a, 2);
  if (countHoles(a) !== 32 || sumDefined(a) !== 992) { check('filled ' + r, countHoles(a) + ':' + sumDefined(a), '32:992'); break; }
}

// a prototype gains an element: holes read it
Array.prototype[5] = 1000;
{
  const a = new Array(64);
  check('Array.prototype element', a[5], 1000);
  check('Array.prototype element in a loop', countHoles(a), 63);
  check('sum through the prototype', sumDefined(a), 1000);
}
delete Array.prototype[5];
Object.prototype[7] = 2000;
check('Object.prototype element', sumDefined(new Array(64)), 2000);
delete Object.prototype[7];
check('plain again', countHoles(new Array(64)), 64);

// an index getter on the prototype, and a proxy as an array's prototype
let gets = 0;
Object.defineProperty(Array.prototype, 9, { get() { gets++; return 5; }, configurable: true });
check('index getter', sumDefined(new Array(64)), 5);
check('getter ran', gets, 1);
delete Array.prototype[9];
const viaProxy = new Array(64);
Object.setPrototypeOf(viaProxy, new Proxy(Array.prototype, { get(t, k, r) { return k === '3' ? 77 : Reflect.get(t, k, r); }, has(t, k) { return k === '3' || Reflect.has(t, k); } }));
check('proxy prototype', sumDefined(viaProxy), 77);

// stores into holes: an index setter on the prototype, frozen, sealed and non-extensible arrays
let sets = 0;
Object.defineProperty(Array.prototype, 4, { set(v) { sets++; }, get() { return undefined; }, configurable: true });
{
  const a = fill(new Array(16), 1);
  check('setter took index 4', sets, 1);
  check('index 4 not own', Object.prototype.hasOwnProperty.call(a, 4), false);
  check('index 5 own', a[5], 5);
}
delete Array.prototype[4];
function strictFill(a) { 'use strict'; for (let i = 0; i < a.length; i++) a[i] = i; }
for (const [name, f] of [['frozen', Object.freeze], ['sealed', Object.seal], ['non-extensible', Object.preventExtensions]]) {
  const a = f(new Array(16));
  let err = 'none';
  try { strictFill(a); } catch (e) { err = e.constructor.name; }
  check(name + ' store throws', err, 'TypeError');
  check(name + ' stays a hole', 0 in a, false);
}

// objects stored into holes survive collections
const keep = [];
for (let r = 0; r < 300; r++) {
  const a = new Array(256);
  for (let i = 0; i < a.length; i++) a[i] = { i, r };
  if (r % 50 === 0) keep.push(a);
}
check('kept objects', keep.map(a => a[255].i + a[0].r).join(), '255,305,355,405,455,505');

console.log(bad === 0 ? 'ok' : 'FAILED ' + bad);
