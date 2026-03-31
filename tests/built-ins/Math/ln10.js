/*---
description: Math.LN10
features: [Math.LN10]
---*/

test("Math.LN10", () => {
  expect(typeof Math.LN10).toBe("number");
  expect(Math.LN10).toBeCloseTo(2.30258, 3);
});
