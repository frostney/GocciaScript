/*---
description: Math object constants have correct values
features: [Math.PI, Math.E, Math.LN2, Math.LN10, Math.SQRT2]
---*/

test("Math constants", () => {
  expect(Math.PI).toBeCloseTo(3.14159, 4);
  expect(Math.E).toBeCloseTo(2.71828, 4);
  expect(Math.LN2).toBeCloseTo(0.69314, 4);
  expect(Math.LN10).toBeCloseTo(2.30258, 4);
  expect(Math.SQRT2).toBeCloseTo(1.41421, 4);
});

runTests();
