// properties added past an object's inline slots by compiled code (the add transition into an overflow array with
// room, its length raised): several orders, values young objects stored into old overflow arrays (the barrier)
// with collections between, reads back, deletes and re-adds, and non-extensible objects refused
let bad = 0;
function check(what, got, want) { if (!Object.is(got, want)) { bad++; console.log('FAIL', what, got, want); } }

function Node(x, y) { this.x = x; this.y = y; }
function decorate(n, i) { n.f = i; n.g = i * 2; n.h = { i }; n.parent = n; n.visited = true; n.closed = i & 1; n.cost = i + 0.5; n.tag = 't' + i; return n; }
function sumBack(ns) { let s = 0; for (const n of ns) s += n.f + n.g + n.h.i + (n.parent === n ? 1 : 0) + (n.visited ? 1 : 0) + n.closed + n.cost + n.tag.length; return s; }

let want = 0;
const kept = [];
for (let r = 0; r < 4; r++) {
  const ns = [];
  for (let i = 0; i < 20000; i++) ns.push(decorate(new Node(i, -i), i));
  if (r === 0) for (let i = 0; i < ns.length; i += 97) kept.push(ns[i]);
  let w = 0; for (let i = 0; i < 20000; i++) w += i + 2 * i + i + 1 + 1 + (i & 1) + i + 0.5 + ('t' + i).length;
  check('round ' + r, sumBack(ns), w);
}
// the kept nodes, old by now: young values stored into their overflow arrays, then collections
function refresh(ns, k) { for (const n of ns) { n.h = { i: k }; n.extra1 = { k }; n.extra2 = [k]; } }
for (let k = 0; k < 50; k++) { refresh(kept, k); for (let j = 0; j < 2000; j++) ({ junk: [j, j] }); }
check('old overflow', kept.every(n => n.h.i === 49 && n.extra1.k === 49 && n.extra2[0] === 49), true);
// delete and add again, a different order of adds, frozen objects
function addOrder(o, a, b, c) { o[a] = 1; o[b] = 2; o[c] = 3; return o; }
let s2 = 0;
for (let i = 0; i < 6000; i++) {
  const o = new Node(1, 2);
  const keys = i % 3 === 0 ? ['a', 'b', 'c'] : i % 3 === 1 ? ['c', 'a', 'b'] : ['b', 'c', 'a'];
  decorate(o, i); addOrder(o, ...keys); delete o.g; o.g = 5;
  s2 += o.a + o.b * 10 + o.c * 100 + o.g;
}
check('orders', s2, 6000 * 5 + 2000 * (1 + 20 + 300) + 2000 * (2 + 30 + 100) + 2000 * (3 + 10 + 200));
check('frozen (sloppy writes fail quietly)', (() => { const o = Object.freeze(new Node(1, 2)); decorate(o, 1); return Object.keys(o).join(); })(), 'x,y');

console.log(bad === 0 ? 'ok' : 'FAILED ' + bad);
