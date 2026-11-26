// strict equality of tagged values in hot loops (strings equal but not the same, doubles with NaN and -0, BigInts,
// objects, mixed kinds) gives what the interpreter gives, and an inlined compare of a property with a string
// constant keeps running optimized
const strs = ['forward', 'back' + 'ward', ['for', 'ward'].join(''), 'f' + 'orward'.slice(0), String(1.5), '1.5'];
const vals = [1, 1.0, 1.5, NaN, -0, 0, 10n, 10n * 1n, 'x', null, undefined, true, {}, strs[0], strs[2]];
function eqCount(a, b, n) { let c = 0; for (let i = 0; i < n; i++) { if (a[i % a.length] === b) c++; if (a[(i + 1) % a.length] !== b) c += 2; } return c; }
class C { constructor(d) { this.direction = d; this.v1 = 1; this.v2 = 2; } out() { return this.direction === 'forward' ? this.v2 : this.v1; } }
function dirs(n) { const cs = [new C('forward'), new C(['for', 'ward'].join('')), new C('backward'), new C('f' + 'orward'.toString())]; let s = 0; for (let i = 0; i < n; i++) s += cs[i & 3].out(); return s; }
const out = [];
for (let r = 0; r < 20; r++) {
  for (const b of vals) out.push(eqCount(vals, b, 300));
  for (const b of strs) out.push(eqCount(strs, b, 300));
  out.push(dirs(4000));
}
let h = 0; const s = out.join(','); for (let i = 0; i < s.length; i++) h = (h * 31 + s.charCodeAt(i)) | 0;
console.log(h === 1897705772 ? 'ok' : 'FAIL ' + h);
