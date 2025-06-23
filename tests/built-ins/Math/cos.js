/*---
description: Math.cos method returns the cosine of a number
features: [Math.cos]
---*/

test("Math.cos", () => {
  expect(Math.cos(0)).toBeCloseTo(1, 10);
  expect(Math.cos(Math.PI / 2)).toBeCloseTo(0, 10);
  expect(Math.cos(Math.PI)).toBeCloseTo(-1, 10);
  expect(Math.cos((3 * Math.PI) / 2)).toBeCloseTo(0, 10);

  expect(Math.cos(NaN)).toBeNaN();
  expect(Math.cos(Infinity)).toBeNaN();
  expect(Math.cos(-Infinity)).toBeNaN();
});
