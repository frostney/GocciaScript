// %TypedArray%.prototype.sort reads each element into a list first; a BigInt element allocates, so the typed array
// is read again from its root after each one (a copy taken once was stale after a collection)
let bad = 0;
for (let k = 0; k < 200; k++) {
  const ta = new BigInt64Array(64);
  for (let i = 0; i < 64; i++) ta[i] = BigInt((i * 7919 + k) % 101) - 50n;
  ta.sort();
  for (let i = 1; i < 64; i++) if (ta[i - 1] > ta[i]) { bad++; break; }
  const junk = [{ k }, [k]];
}
console.log(bad === 0 ? "ok" : "bad " + bad);
