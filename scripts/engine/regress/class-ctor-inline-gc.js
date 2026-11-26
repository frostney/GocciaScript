// new of class constructors inlined into hot loops (a base class, one with fields, a derived one through super; an
// implicit derived one called): the inline allocations hold the young objects they are given across collections (run
// with a small nursery)
class P { constructor(x, o) { this.x = x; this.o = o; } }
class F { a = 1; b = 2; constructor(v, o) { this.v = v; this.o = o; } }
class B { constructor(k) { this.k = k; } }
class D extends B { constructor(k) { super(k + 1); this.d = k; } }
class E extends B { extra = 7; }
function run(n, r, keep) {
  let ok = 0;
  for (let i = 0; i < n; i++) {
    const o = { v: i, r };
    const p = new P(i, o); const f = new F(i, { w: i }); const d = new D(i);
    if (i % 7 === 0) keep.push(p, f, d);
    if (p.o.v === i && f.o.w === i && f.a + f.b === 3 && d.k === i + 1 && d.d === i) ok++;
  }
  return ok;
}
function runE(n, keepE) {
  let ok = 0;
  for (let i = 0; i < n; i++) {
    const e = new E({ i });
    if (i % 7 === 0) keepE.push(e);
    if (e.k.i === i && e.extra === 7) ok++;
  }
  return ok;
}
const keep = [], keepE = [];
let ok = 0, n = 0;
for (let r = 0; r < 8; r++) { ok += run(5000, r, keep) + runE(5000, keepE); n += 10000; }
for (let j = 0; j < keep.length; j += 3) {
  const i = keep[j].x;
  n++;
  if (keep[j].o.v === i && keep[j + 1].o.w === i && keep[j + 2].d === i && keep[j + 2] instanceof B) ok++;
}
for (let j = 0; j < keepE.length; j++) {
  n++;
  if (keepE[j].k.i === (j % 715) * 7 && keepE[j].extra === 7 && Object.getPrototypeOf(keepE[j]) === E.prototype) ok++;
}
console.log(ok === n ? 'ok' : 'FAIL ' + (n - ok));
