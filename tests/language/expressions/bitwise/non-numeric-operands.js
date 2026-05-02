/*---
description: |
  Non-numeric operands (undefined, null, objects) coerce through
  ToInt32 / ToUint32 per ES2026 §7.1.6 / §7.1.7.  ToInt32(NaN), ToInt32(±0),
  and ToInt32(±Infinity) all yield 0; bitwise operators must therefore
  treat undefined / null / non-numeric objects as 0 before applying the bit op.

  This test guards against a regression where bare Pascal Trunc(NaN) was used
  to coerce, which on FPC's x86_64 backend returns Int64.MinValue (the SSE
  cvttsd2si "indefinite" sentinel) instead of 0 — surfacing as
  `(undefined & undefined) === -9.22e18` on Linux x86_64 CI while passing on
  arm64.  Also covers the test262 cluster
  language/expressions/{bitwise-and,bitwise-or,bitwise-xor,bitwise-not,
  left-shift,compound-assignment} that drove the discovery.
features: [bitwise-and, bitwise-or, bitwise-xor, bitwise-not, left-shift, right-shift, unsigned-right-shift]
---*/

test("undefined and null coerce to 0 in bitwise &, |, ^", () => {
  expect(undefined & undefined).toBe(0);
  expect(null & undefined).toBe(0);
  expect(undefined | undefined).toBe(0);
  expect(null | undefined).toBe(0);
  expect(undefined ^ undefined).toBe(0);
  expect(null ^ undefined).toBe(0);
});

test("plain objects and functions coerce to 0 in bitwise &, |, ^", () => {
  // {} → ToNumber → NaN → ToInt32 → 0
  expect(({}) & ({})).toBe(0);
  expect(({}) | ({})).toBe(0);
  expect(({}) ^ (() => {})).toBe(0);
});

test("~undefined and ~null are -1", () => {
  expect(~undefined).toBe(-1);
  expect(~null).toBe(-1);
  expect(~({})).toBe(-1);
  expect(~(() => {})).toBe(-1);
});

test("shifts with non-numeric operands coerce to 0", () => {
  expect(undefined << 1).toBe(0);
  expect(undefined >> 1).toBe(0);
  expect(undefined >>> 1).toBe(0);
  expect(1 << undefined).toBe(1);  // shift count NaN → 0, no shift
  expect(8 >> undefined).toBe(8);
  expect(8 >>> undefined).toBe(8);
  expect(null << null).toBe(0);
});

test("Infinity and -Infinity coerce to 0 in bitwise contexts", () => {
  expect(Infinity & 1).toBe(0);
  expect((-Infinity) | 0).toBe(0);
  expect(NaN ^ 0).toBe(0);
  expect(~Infinity).toBe(-1);
  expect(~NaN).toBe(-1);
});

test("compound bitwise assignment with non-numeric values", () => {
  let x = undefined;
  x &= 5;
  expect(x).toBe(0);

  let y = null;
  y |= 7;
  expect(y).toBe(7);

  let z = undefined;
  z ^= null;
  expect(z).toBe(0);

  let a = undefined;
  a <<= 3;
  expect(a).toBe(0);
});

test("finite numeric operands are unaffected by the helper", () => {
  expect(5 & 3).toBe(1);
  expect(5 | 2).toBe(7);
  expect(5 ^ 6).toBe(3);
  expect(~5).toBe(-6);
  expect(1 << 30).toBe(1073741824);
  expect(8 >> 2).toBe(2);
  expect((-1) >>> 0).toBe(4294967295);
});
