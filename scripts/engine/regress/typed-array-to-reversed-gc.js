// goccia-flags: --gc-stress --gc-verify
// BigInt64Array.prototype.toReversed: each read allocates its BigInt and can collect, so the new array it fills must
// be rooted across the read; a stale copy would store into a moved or freed array.
let ok = 0;
const n = 200;
for (let r = 0; r < n; r++) {
  const keep = [];
  for (let i = 0; i < 8; i++) keep.push({ i, a: [i, i] });
  const TA = r % 2 === 0 ? BigInt64Array : BigUint64Array;
  const src = new TA([10n, 20n, 30n, BigInt(r + 1)]);
  const out = src.toReversed();
  if (out instanceof TA && out.length === 4 && out[0] === BigInt(r + 1) && out[1] === 30n && out[2] === 20n &&
      out[3] === 10n) ok++;
}
console.log(ok === n ? 'ok' : 'FAIL ' + (n - ok));
