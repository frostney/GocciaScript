// four levels of calls inlined into an optimized loop (a hash map's put and get down to its key comparison): the
// deepest callee's guards failing (other key kinds after ints), caller and arguments read from the deepest level
// while it runs inlined, a throw from it caught by the loop, and the chain entered recursively
let bad = 0;
function check(what, got, want) { if (!Object.is(got, want)) { bad++; console.log('FAIL', what, got, want); } }

let note = null;
function record(f) { note = [f.caller && f.caller.name, Array.prototype.slice.call(f.arguments).join()]; }
function same(a, b) {
  if (typeof a != typeof b) return false;
  if (a === -1) record(same);
  if (a === -2) throw new RangeError('deep');
  return a === b;
}
function find(keys, k) { for (let i = 0; i < keys.length; i++) if (same(keys[i], k)) return i; return -1; }
function lookup(m, k) { return find(m.keys, k); }
function get(m, k) { const i = lookup(m, k); return i < 0 ? undefined : m.vals[i]; }
function sumGets(m, n) { let s = 0; for (let i = 0; i < n; i++) { const v = get(m, i % 8); if (v !== undefined) s += v; } return s; }

const m = { keys: [0, 1, 2, 3, 4, 5, 6, 7], vals: [10, 11, 12, 13, 14, 15, 16, 17] };
for (let r = 0; r < 3; r++) check('ints ' + r, sumGets(m, 8000), 1000 * (10 + 11 + 12 + 13 + 14 + 15 + 16 + 17));
// other key kinds at the deepest level
const m2 = { keys: ['a', 1.5, null, 3, {}, 5, true, 7], vals: [1, 2, 3, 4, 5, 6, 7, 8] };
check('mixed keys', sumGets(m2, 8000), 1000 * (4 + 6 + 8));
check('string key', get(m2, 'a'), 1);
check('double key', get(m2, 1.5), 2);
// caller and arguments read from the deepest level
const m3 = { keys: [0, -1, 2], vals: [1, 2, 3] };
sumGets(m3, 3000);
check('caller and arguments', note && note.join('|'), 'find|-1,7');
// a throw from the deepest level
const m4 = { keys: [0, 1, -2], vals: [1, 2, 3] };
let caught = 0;
function sumCatch(m, n) { let s = 0; for (let i = 0; i < n; i++) { try { s += get(m, i % 8) || 0; } catch (e) { if (e instanceof RangeError) caught++; } } return s; }
sumGets(m, 3000);
check('throw from deepest', sumCatch(m4, 3000) + ':' + caught, '1125:2250');
// the chain entered recursively through a key's own comparison
const rec = { keys: [0], vals: [1] };
function deepGet(d) { return d === 0 ? get(rec, 0) : deepGet(d - 1) + get(m, d % 8); }
check('recursive', deepGet(40), 1 + [...Array(40)].reduce((s, _, i) => s + m.vals[(i + 1) % 8], 0));

console.log(bad === 0 ? 'ok' : 'FAILED ' + bad);
