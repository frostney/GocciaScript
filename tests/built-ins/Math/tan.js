/*---
description: Math.tan method returns the tangent of a number
features: [Math.tan]
---*/

test("Math.tan", () => {
  expect(Math.tan(0)).toBeCloseTo(0, 10);
  expect(Math.tan(Math.PI / 4)).toBeCloseTo(1, 10);
  expect(Math.tan(Math.PI)).toBeCloseTo(0, 10);
  expect(Math.tan(-Math.PI / 4)).toBeCloseTo(-1, 10);

  expect(Math.tan(NaN)).toBeNaN();
  expect(Math.tan(Infinity)).toBeNaN();
  expect(Math.tan(-Infinity)).toBeNaN();
});
