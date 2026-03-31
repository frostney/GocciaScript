/*---
description: Math.SQRT1_2
features: [Math.SQRT1_2]
---*/

test("Math.SQRT1_2", () => {
  expect(typeof Math.SQRT1_2).toBe("number");
  expect(Math.SQRT1_2).toBeCloseTo(0.70710, 3);
});
