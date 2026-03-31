/*---
description: Math.LN2
features: [Math.LN2]
---*/

test("Math.LN2", () => {
  expect(typeof Math.LN2).toBe("number");
  expect(Math.LN2).toBeCloseTo(0.69314, 3);
});
