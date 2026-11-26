// feedback kept per function (shared by every region that inlines it) and number properties learned per shape: a
// class whose fields start as ints and later hold doubles, read by several inlining callers; ints read as doubles
// then used as indices, in bit operations and in comparisons; and strings, undefined and objects later stored in
// the same fields of the same shape
let bad = 0;
function check(what, got, want) { if (!Object.is(got, want)) { bad++; console.log('FAIL', what, got, want); } }

class V {
  x = 0; y = 0; z = 0;
  constructor(x, y, z) { this.x = x; this.y = y; this.z = z; }
  static add(a, b) { return new V(a.x + b.x, a.y + b.y, a.z + b.z); }
  static sub(a, b) { return new V(a.x - b.x, a.y - b.y, a.z - b.z); }
  static scale(a, k) { return new V(a.x * k, a.y * k, a.z * k); }
  dot(b) { return this.x * b.x + this.y * b.y + this.z * b.z; }
  limit() { this.x = this.x > 0 ? (this.x > 1 ? 1 : this.x) : 0; return this; }
}

function walk(n, step) {
  let p = new V(0, 0, 0), s = 0;
  for (let i = 0; i < n; i++) {
    p = V.add(p, V.scale(new V(1, 2, 3), step));
    s += V.sub(p, new V(1, 1, 1)).dot(p);
  }
  return s;
}
function indexWith(arr, vs) { let s = 0; for (const v of vs) s += arr[v.x] + (v.y | 0) + (v.z > 1 ? 1 : 0); return s; }
function limits(vs) { let s = 0; for (const v of vs) s += v.limit().x; return s; }

// ints first, then doubles: the same callers and callees learn the doubles once
let want = 0;
for (let i = 0, p = 0; i < 300; i++) { p += 1; want += (p - 1) * p + (2 * p - 1) * 2 * p + (3 * p - 1) * 3 * p; }
for (let r = 0; r < 30; r++) check('int walk ' + r, walk(300, 1), want);
check('double walk', walk(4, 0.5), (0.5 - 1) * 0.5 + 0 * 1 + (1.5 - 1) * 1.5
  + (1 - 1) * 1 + (2 - 1) * 2 + (3 - 1) * 3
  + (1.5 - 1) * 1.5 + (3 - 1) * 3 + (4.5 - 1) * 4.5
  + (2 - 1) * 2 + (4 - 1) * 4 + (6 - 1) * 6);
for (let r = 0; r < 30; r++) walk(300, 0.25);
for (let r = 0; r < 30; r++) check('int walk again ' + r, walk(300, 1), want);

// ints in fields of a shape whose fields have held doubles: indices, bit operations, comparisons
const arr = Array.from({ length: 10 }, (_, i) => i * 10);
const ints = Array.from({ length: 1000 }, (_, i) => new V(i % 10, i % 7, i % 3));
const iw = ints.reduce((s, v) => s + v.x * 10 + v.y + (v.z > 1 ? 1 : 0), 0);
for (let r = 0; r < 50; r++) check('index ' + r, indexWith(arr, ints), iw);
check('double index', indexWith(arr, [new V(1.5, 2.5, 2.5)]), undefined + 2 + 1);
check('limit ints', limits(Array.from({ length: 1000 }, (_, i) => new V(i % 3 - 1, 0, 0))), 333);
check('limit doubles', limits([new V(0.5, 0, 0), new V(-0.5, 0, 0), new V(2.5, 0, 0)]), 1.5);
for (let r = 0; r < 50; r++) limits(Array.from({ length: 100 }, (_, i) => new V(i / 50, 0, 0)));
check('limit after', limits([new V(0.25, 0, 0), new V(3, 0, 0)]), 1.25);

// other values in the same fields of the same shape
check('strings', V.add(new V('a', 'b', 'c'), new V(1, 2, 3)).y, 'b2');
check('undefined', Number.isNaN(new V(undefined, 0, 0).dot(new V(1, 1, 1))), true);
check('objects', V.sub(new V({ valueOf() { return 5; } }, 0, 0), new V(1.5, 0, 0)).x, 3.5);
check('walk after', walk(300, 1), want);
check('negative zero', Object.is(V.scale(new V(0, 1, 1), -1).x, -0), true);

console.log(bad === 0 ? 'ok' : 'FAILED ' + bad);
