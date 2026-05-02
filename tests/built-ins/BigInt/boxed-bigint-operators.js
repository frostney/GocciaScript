/*---
description: |
  Operands of bitwise and unary arithmetic operators must go through
  ToPrimitive before the IsBigInt branch check, so a boxed BigInt
  (`Object(1n)`) is unboxed to its primitive BigInt and takes the
  BigInt-typed path instead of being silently coerced to a Number
  via the boxed object's ToNumberLiteral.

  Guards against a regression where the shared operator helpers and VM
  fallbacks called `is TGocciaBigIntValue` directly on the un-unboxed
  operand: boxed BigInts missed the BigInt branch and produced
  Number 0 / -1 results from the NaN-coercion fall-through.
features: [bigint]
---*/

test("boxed BigInt operands keep BigInt type in & | ^", () => {
  const a = Object(0b1100n);
  const b = Object(0b1010n);
  expect(typeof (a & b)).toBe("bigint");
  expect(a & b).toBe(0b1000n);
  expect(typeof (a | b)).toBe("bigint");
  expect(a | b).toBe(0b1110n);
  expect(typeof (a ^ b)).toBe("bigint");
  expect(a ^ b).toBe(0b0110n);
});

test("boxed BigInt operands keep BigInt type in shifts", () => {
  const a = Object(12n);
  expect(typeof (a << 1n)).toBe("bigint");
  expect(a << 1n).toBe(24n);
  expect(typeof (a >> 1n)).toBe("bigint");
  expect(a >> 1n).toBe(6n);
});

test("boxed BigInt unary ~ stays BigInt", () => {
  const a = Object(12n);
  expect(typeof ~a).toBe("bigint");
  expect(~a).toBe(-13n);
});

test("boxed BigInt unary - stays BigInt", () => {
  const a = Object(10n);
  expect(typeof -a).toBe("bigint");
  expect(-a).toBe(-10n);
});

test("mixed boxed BigInt + Number throws TypeError", () => {
  const a = Object(1n);
  expect(() => a & 1).toThrow(TypeError);
  expect(() => a | 1).toThrow(TypeError);
  expect(() => a ^ 1).toThrow(TypeError);
  expect(() => a << 1).toThrow(TypeError);
});

test("boxed BigInt >>> always throws (no unsigned right shift on BigInt)", () => {
  const a = Object(1n);
  expect(() => a >>> 1n).toThrow(TypeError);
});

test("primitive BigInt continues to work after the unbox path", () => {
  // Sanity: the ToPrimitive fast path (already-primitive values
  // return AS-IS) must not regress the common case.
  expect(0b1100n & 0b1010n).toBe(0b1000n);
  expect(0b1100n | 0b1010n).toBe(0b1110n);
  expect(~12n).toBe(-13n);
  expect(-10n).toBe(-10n);
  expect(12n << 1n).toBe(24n);
});
