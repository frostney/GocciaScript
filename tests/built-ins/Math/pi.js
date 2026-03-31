/*---
description: Math.PI
features: [Math.PI]
---*/

test("Math.PI", () => {
  expect(typeof Math.PI).toBe("number");
  expect(Math.PI).toBeCloseTo(3.14159, 3);
});
