/*---
description: Number.MAX_VALUE
features: [Number.MAX_VALUE]
---*/

test("Number.MAX_VALUE", () => {
  expect(Number.MAX_VALUE > 0).toBe(true);
  expect(Number.isFinite(Number.MAX_VALUE)).toBe(true);
});
