/*---
description: Math.tan method returns the tangent of a number
features: [Math.tan]
---*/

test("Math.tan", () => {
  expect(Math.tan(0)).toBe(0);
  expect(Object.is(Math.tan(-0), -0)).toBe(true);
  expect(Math.tan(Math.PI / 4)).toBeCloseTo(1, 10);
  expect(Math.tan(Math.PI)).toBeCloseTo(0, 10);
  expect(Math.tan(-Math.PI / 4)).toBeCloseTo(-1, 10);

  expect(Math.tan(NaN)).toBeNaN();
  expect(Math.tan(Infinity)).toBeNaN();
  expect(Math.tan(-Infinity)).toBeNaN();
});

test("Math.tan coerces its argument", () => {
  expect(Math.tan("0")).toBe(0);
  expect(() => Math.tan(1n)).toThrow(TypeError);
});
