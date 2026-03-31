/*---
description: Math.E
features: [Math.E]
---*/

test("Math.E", () => {
  expect(typeof Math.E).toBe("number");
  expect(Math.E).toBeCloseTo(2.71828, 3);
});
