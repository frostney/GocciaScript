/*---
description: Addition operation with numeric operands
features: [addition-operator]
---*/

test("adds positive and negative finite numbers", () => {
  expect(2 + 3).toBe(5);
  expect(+10 + 20).toBe(30);
  expect(-5 + 3).toBe(-2);
  expect(5 + -3).toBe(2);
  expect(-5 + -3).toBe(-8);
});

test("preserves the numeric identities and special values", () => {
  expect(5 + 0).toBe(5);
  expect(0 + 7).toBe(7);
  expect(Object.is(-0 + -0, -0)).toBe(true);
  expect(Object.is(-0 + 0, 0)).toBe(true);
  expect(Infinity + 1).toBe(Infinity);
  expect(-Infinity + 1).toBe(-Infinity);
  expect(Number.isNaN(Infinity + -Infinity)).toBe(true);
  expect(Number.isNaN(NaN + 1)).toBe(true);
});
