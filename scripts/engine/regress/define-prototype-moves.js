// Object.defineProperty(F, "prototype", { value: P }) on a function whose prototype was not made yet: the define
// first makes the default one (an allocation), and P, read from the descriptor before it, must be read again after
// (a collection moves a young P: F.prototype was left pointing where P had been)
let bad = 0;
for (let k = 0; k < 3000; k++) {
  const F = function () {};
  const P = { k, pad: [k, k, k, k] };
  Object.defineProperty(F, "prototype", { value: P, writable: false });
  const junk = [{ a: k }, { b: k }, "x" + k];
  if (F.prototype !== P || F.prototype.k !== k || junk.length !== 3) { bad++; if (bad < 5) console.log("FAIL", k); }
}
console.log(bad === 0 ? "ok" : "bad " + bad);
