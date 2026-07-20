/*---
description: Decimal numeric literals round directly to the nearest binary64 value
features: [numeric-literals]
---*/

test("decimal numeric literals use round-to-nearest ties-to-even", () => {
  expect(0.30000000000000004).toBe(0.1 + 0.2);
  expect(9007199254740993).toBe(9007199254740992);
  expect(2.4703282292062327e-324).toBe(0);
  expect(2.4703282292062328e-324).toBe(Number.MIN_VALUE);
  expect(1.7976931348623158e308).toBe(Number.MAX_VALUE);
  expect(1.7976931348623159e308).toBe(Infinity);
  expect(0x20000000000001).toBe(9007199254740992);
});
