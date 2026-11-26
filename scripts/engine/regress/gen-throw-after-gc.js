// A compiled generator body that allocates enough to run collections and then throws: the generator moved, and
// the compiled step's bail-out used to mark its stale copy, so the real one stayed "already running" (next() threw)
// instead of done. With the interpreter and with the JIT every run must print { done: true } after the throw.
function* gen(n) { for (let i = 0; i < n; i++) { const o = { i, a: [i, i, i] }; } if (n > 0) throw new Error("boom"); yield 1; }
function consume(n) {
  const g = gen(n);
  let c = "";
  try { for (const x of g) { c += x; } } catch (e) { c += e.message; }
  let r; try { r = JSON.stringify(g.next()); } catch (e) { r = "E:" + e.message; }
  return c + " " + r;
}
for (let i = 0; i < 30; i++) consume(0);
let bad = 0;
for (let i = 0; i < 30; i++) { const s = consume(20000); if (s !== 'boom {"done":true}') { bad++; if (bad < 3) console.log('FAIL', s); } }
console.log(bad === 0 ? 'ok' : 'FAIL ' + bad);
