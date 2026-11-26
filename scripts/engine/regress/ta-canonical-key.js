// a typed array's [[GetOwnProperty]] for a numeric-looking key ("1e3", "+1", "Infinityx") first tests
// CanonicalNumericIndexString, then looks the key up in the shape; the test built a heap string, and the object
// pointer read after it was stale when that allocation collected
let bad = 0;
const names = ["1e3", "1e21", "01", "+1", " 1", ".5", "0x1", "Infinityx", "NaNa", "-", "--1", "-foo", "I", "N", "label"];
for (let k = 0; k < 3000; k++) {
  const ta = new Uint8Array([7, 8, 9]);
  ta.label = k;
  for (const name of names) {
    if (Object.hasOwn(ta, name) !== (name === "label")) { bad++; if (bad < 5) console.log("FAIL", k, name); }
  }
}
console.log(bad === 0 ? "ok" : "bad " + bad);
