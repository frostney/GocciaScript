// a native call lowers StackTop to its last argument, so the frames it makes reuse the caller's registers above
// it; a collection during the call neither scans nor updates them. Once the call returns they would hold stale
// pointers the next collection follows: String's register is loaded before the await, the 100000 then() calls
// run Promise.prototype.then natively with collections inside, and the closures made in between allocate
const keep = Array.from({ length: 1000 }, (_, i) => ({ i, s: 'x' + i }));
const range = (n) => Array.from({ length: n });
(async () => {
  let resolve;
  const first = new Promise((r) => { resolve = r; });
  let last = first;
  for (const _ of range(100000)) last = last.then((v) => v + 1);
  resolve(0);
  console.log(String(await last) === "100000" ? "ok" : "bad");
})();
