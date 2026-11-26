// stores of young objects into properties held in overflow slots (objects grown past their inline capacity) are
// remembered: a collection updates them (run with a small nursery)
function mk(i) { const o = { a: i }; o.b = 1; o.c = 2; o.d = 3; o.e = 4; o.f = 5; o.g = 6; return o; }
const os = []; for (let i = 0; i < 3000; i++) os.push(mk(i));
function store(a, r) { for (let i = 0; i < a.length; i++) a[i].g = { v: i + r, pad: [i, i + 1] }; }
let ok = 0, n = 0;
for (let r = 0; r < 40; r++) {
  store(os, r);
  for (let i = 0; i < os.length; i++) { n++; if (os[i].g.v === i + r && os[i].g.pad[1] === i + 1) ok++; }
}
console.log(ok === n ? 'ok' : 'FAIL ' + (n - ok));
