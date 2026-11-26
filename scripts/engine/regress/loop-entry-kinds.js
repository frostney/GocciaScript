// a loop region entered with a register whose kind changes between calls (an index one call, a name the
// next, as babel's NodePath.get key) keeps giving the interpreter's results, and so do registers used as
// numbers in the loop or after it through a move
let bad = 0;
function check(what, got, want) { if (!Object.is(got, want)) { bad++; console.log('FAIL', what, got, want); } }
function find(list, target, key, extra) {
  let found = null;
  for (let i = 0; i < list.length; i++) {
    if (list[i].node === target) { found = list[i]; break; }
  }
  const k2 = key;
  return [found ? found.id : -1, typeof key, k2, extra + 1];
}
const list = [];
for (let i = 0; i < 40; i++) list.push({ node: { n: i }, id: i });
const keys = [3, 'body', 2.5, { o: 1 }, -7, 'params', null, undefined, true, 1e10];
let s = '';
for (let r = 0; r < 3000; r++) {
  const key = keys[r % keys.length];
  const extra = r % 3 === 0 ? 1.5 : r % 3 === 1 ? 7 : 'x';
  const res = find(list, list[r % 40].node, key, extra);
  if (r % 97 === 0) s += res.map(String).join('|') + ';';
  check('id ' + r, res[0], r % 40);
  check('key ' + r, res[2], key);
}
function counter(n, start) { let c = start; for (let i = 0; i < n; i++) c = c + 1; return c; }
let t = 0;
for (let r = 0; r < 2000; r++) { const st = r % 2 ? r : r + 0.5; t += counter(5, st); }
let tw = 0;
for (let r = 0; r < 2000; r++) tw += (r % 2 ? r : r + 0.5) + 5;
check('numbers through the loop', t, tw);
check('trace', s.length > 0, true);
console.log(bad === 0 ? 'ok' : 'FAIL ' + bad);
