/*---
description: Math.LOG10E
features: [Math.LOG10E]
---*/

test("Math.LOG10E", () => {
  expect(typeof Math.LOG10E).toBe("number");
  expect(Math.LOG10E).toBeCloseTo(0.43429, 3);
});
