// strict equality of tagged values in optimized loops: objects (the same and different), strings with equal
// contents in different cells, ints, doubles, NaN, -0, booleans, undefined, null, symbols and BigInts, each
// against each other on either side, and objects against strings with the object on either side
let bad = 0;
function check(what, got, want) { if (!Object.is(got, want)) { bad++; console.log('FAIL', what, got, want); } }

const o1 = { a: 1 }, o2 = { a: 1 }, f = () => 1, arr = [];
const sym = Symbol('s');
const s1 = 'ab' + 'cd'.slice(0, 1), s2 = ['a', 'b', 'c'].join('');
const vals = [o1, o2, f, arr, s1, s2, 'abd', 1, 1.5, 0, -0, NaN, true, false, undefined, null, sym, 10n, 10n * 1n, 1e300];

function countEq(xs, ys, n) {
  let eq = 0, ne = 0;
  for (let r = 0; r < n; r++)
    for (let i = 0; i < xs.length; i++) {
      const x = xs[i], y = ys[(i + r) % ys.length];
      if (x === y) eq++;
      if (x !== y) ne++;
    }
  return eq + ':' + ne;
}
function find(list, target) { for (let i = 0; i < list.length; i++) if (list[i].node === target) return i; return -1; }

function slowCount(xs, ys, n) {
  let eq = 0, ne = 0;
  for (let r = 0; r < n; r++)
    for (let i = 0; i < xs.length; i++) {
      const x = xs[i], y = ys[(i + r) % ys.length];
      const nan = typeof x === 'number' && Number.isNaN(x);
      const zeros = typeof x === 'number' && typeof y === 'number' && x == 0 && y == 0;
      if ((Object.is(x, y) && !nan) || zeros) eq++;
      else ne++;
    }
  return eq + ':' + ne;
}

for (let round = 0; round < 3; round++) {
  // objects only first (the loop optimized on them), then every kind
  const objs = [o1, o2, f, arr, o1];
  check('objects ' + round, countEq(objs, objs, 2000), slowCount(objs, objs, 2000));
  check('all ' + round, countEq(vals, vals, 400), slowCount(vals, vals, 400));
  check('objects vs strings ' + round, countEq([o1, s1, o2, s2], [s2, o1, s1, o2], 1000), slowCount([o1, s1, o2, s2], [s2, o1, s1, o2], 1000));
  const nodes = Array.from({ length: 300 }, (_, i) => ({ node: i % 2 ? { k: i } : 'n' + i }));
  let s = 0;
  for (let k = 0; k < 300; k++) s += find(nodes, nodes[k].node);
  check('find ' + round, s, 299 * 300 / 2);
  check('find string ' + round, find(nodes, 'n' + 4), 4);
  check('find missing ' + round, find(nodes, {}), -1);
}
check('-0 and 0', countEq([0], [-0], 10), '10:0');
check('NaN', countEq([NaN], [NaN], 10), '0:10');

console.log(bad === 0 ? 'ok' : 'FAILED ' + bad);
