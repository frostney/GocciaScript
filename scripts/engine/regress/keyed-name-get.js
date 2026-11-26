// obj[name] in compiled code through the megamorphic cache: many shapes and dictionary-mode objects, names found
// own and up the chain, absent names (undefined through the whole chain), names that look numeric with a typed
// array on the chain ('1', '-0', 'NaN', 'Infinity'), accessors, proxies, arguments objects, String objects,
// strings built at run time (not interned yet), and objects and prototypes changed after the cache held them
let bad = 0;
function check(what, got, want) { if (!Object.is(got, want)) { bad++; console.log('FAIL', what, got, want); } }

function get(o, k) { return o[k]; }
function sumKeys(objs, keys, n) {
  let s = 0;
  for (let r = 0; r < n; r++)
    for (let i = 0; i < objs.length; i++) {
      const v = get(objs[i], keys[(i + r) % keys.length]);
      s += v === undefined ? 1000 : typeof v === 'number' ? v : 7;
    }
  return s;
}
function slowSum(objs, keys, n) {
  let s = 0;
  for (let r = 0; r < n; r++)
    for (let i = 0; i < objs.length; i++) {
      const v = Reflect.get(objs[i], keys[(i + r) % keys.length]);
      s += v === undefined ? 1000 : typeof v === 'number' ? v : 7;
    }
  return s;
}

const keys = ['alpha', 'beta', 'gamma', 'delta', 'missing'];
const shapes = [];
for (let i = 0; i < 12; i++) { const o = {}; o['f' + i] = i; o.alpha = 1; if (i & 1) o.beta = 2; if (i & 2) o.gamma = 3; shapes.push(o); }
const dict = {}; for (let i = 0; i < 64; i++) dict['k' + i] = i; for (let i = 0; i < 60; i++) delete dict['k' + i];
dict.alpha = 11; dict.delta = 14;
const protoDict = Object.create(dict);
class P { } P.prototype.beta = 22; class Q extends P { }
const objs = shapes.concat([dict, protoDict, new Q(), Object.create(null)]);
for (let round = 0; round < 3; round++) {
  check('names ' + round, sumKeys(objs, keys, 300), slowSum(objs, keys, 300));
  dict.gamma = 33 + round;                     // a dictionary changed in place
  P.prototype.delta = round;                   // a prototype changed
  shapes[3].beta = round;                      // an object changed
}

// accessors, proxies, arguments, String objects, typed arrays on the chain with numeric-looking names
let calls = 0;
const acc = { get alpha() { calls++; return 5; } };
const prox = new Proxy({ alpha: 6 }, { get(t, k) { calls++; return t[k]; } });
const args = (function () { return arguments; })(1, 2);
const sobj = new String('abc');
const ta = new Uint8Array(4); const overTa = Object.create(ta);
const odd = [acc, prox, args, sobj, overTa, { alpha: 1 }];
const oddKeys = ['alpha', '1', '-0', 'NaN', 'Infinity', '0'];
check('odd receivers', sumKeys(odd, oddKeys, 200), slowSum(odd, oddKeys, 200));
check('accessor calls', calls > 0, true);

// keys built at run time (no atom yet), symbols, and integer-like strings on plain objects
const runtime = [{ ['x' + 1]: 1, ['y' + 2]: 2 }, { x1: 3 }];
check('runtime keys', sumKeys(runtime, ['x' + '1', 'y' + '2', 'z'], 300), slowSum(runtime, ['x1', 'y2', 'z'], 300));
const s1 = Symbol('s');
check('symbol key', sumKeys([{ [s1]: 4 }, { a: 1 }], [s1], 50), slowSum([{ [s1]: 4 }, { a: 1 }], [s1], 50));
check('index-like', sumKeys([{ 5: 9, a: 1 }, [1, 2, 3, 4, 5, 6]], ['5', 'a'], 200), slowSum([{ 5: 9, a: 1 }, [1, 2, 3, 4, 5, 6]], ['5', 'a'], 200));

console.log(bad === 0 ? 'ok' : 'FAILED ' + bad);
