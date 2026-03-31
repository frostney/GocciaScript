/*---
description: Math.SQRT2
features: [Math.SQRT2]
---*/

test("Math.SQRT2", () => {
  expect(typeof Math.SQRT2).toBe("number");
  expect(Math.SQRT2).toBeCloseTo(1.41421, 3);
});
