/*---
description: Math.LOG2E
features: [Math.LOG2E]
---*/

test("Math.LOG2E", () => {
  expect(typeof Math.LOG2E).toBe("number");
  expect(Math.LOG2E).toBeCloseTo(1.44269, 3);
});
