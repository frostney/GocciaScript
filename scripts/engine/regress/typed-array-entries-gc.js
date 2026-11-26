// goccia-flags: --gc-stress --gc-verify
// A BigInt64Array's entries iterator: each step reads the element, which allocates its BigInt and can collect,
// moving the iterator. The step then checked the iterator's kind through its stale copy, and could hand out the
// bare element in place of the [index, value] pair.
let ok = 0;
const n = 400;
for (let r = 0; r < n; r++) {
  const keep = [];
  for (let i = 0; i < 8; i++) keep.push({ i, a: [i, i] });
  const TA = r % 2 === 0 ? BigInt64Array : BigUint64Array;
  const it = new TA([10n, 20n, BigInt(r)]).entries();
  const a = it.next().value, b = it.next().value, c = it.next().value;
  if (Array.isArray(a) && a[0] === 0 && a[1] === 10n && b[0] === 1 && b[1] === 20n && c[0] === 2 && c[1] === BigInt(r)) ok++;
}
console.log(ok === n ? 'ok' : 'FAIL ' + (n - ok));
