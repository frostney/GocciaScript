// generator bodies whose loops run in optimized regions between their yields (a return, a throw, yield*,
// try/finally and a closure among them): each run of the loops gives what the interpreter gives
function* g1(n) { let c = 1; while (true) { const k = (c * 7) % 50 + 1; c++; const a = new Uint8Array(k); let s = 0; for (let i = 0; i < k; i++) { a[i] = (i + c) % 256; s += a[i]; } yield s; if (c > n) return c; } }
function* g2(n) { for (let i = 0; i < n; i++) { let t = 0; for (let j = 0; j < 100; j++) t += j * i; if (t % 3 === 0) yield t; } return 'done'; }
function* g3(arr) { for (const x of arr) { let q = x; for (let j = 0; j < 50; j++) q = (q * 3 + j) | 0; yield q; } }
function* g4(n) { yield* g2(n); let z = 0; for (let i = 0; i < 2000; i++) { z += i; if (i === 1500) { try { yield z; } finally { z = -z; } } } return z; }
function* g5(n) { let k = 0; for (let i = 0; i < 3000; i++) { k += i; if (i === n) throw new Error('boom ' + k); } yield k; }
function* g6() { let x = 0; const f = () => x; for (let i = 0; i < 5000; i++) x += i; yield f(); x = 1; yield f(); }
const out = [];
for (let r = 0; r < 30; r++) {
  let s = 0; const it = g1(40); let v; while (!(v = it.next()).done) s += v.value; out.push(s, v.value);
  let s2 = 0, last; for (const t of g2(60)) s2 += t; const it2 = g2(10); for (;;) { const q = it2.next(); if (q.done) { last = q.value; break; } } out.push(s2, last);
  out.push([...g3([1, 2, 3, 4, 5, r])].join());
  const it4 = g4(9); let s4 = 0, q4; while (!(q4 = it4.next()).done) s4 += q4.value; out.push(s4, q4.value);
  const it4b = g4(3); for (let k = 0; k < 4; k++) it4b.next(); out.push(JSON.stringify(it4b.return(7)));
  try { for (const _ of g5(2500)) {} } catch (e) { out.push(e.message); }
  out.push([...g6()].join());
  const it1 = g1(5); it1.next(); out.push(JSON.stringify(it1.return(3)), JSON.stringify(it1.next()));
}
let h = 0; const str = out.join('|'); for (let i = 0; i < str.length; i++) h = (h * 31 + str.charCodeAt(i)) | 0;
console.log(h === -54240951 ? 'ok' : 'FAIL ' + h);
