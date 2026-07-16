/*---
description: Math.sin method returns the sine of a number
features: [Math.sin]
---*/

test("Math.sin", () => {
  expect(Math.sin(0)).toBe(0);
  expect(Object.is(Math.sin(-0), -0)).toBe(true);
  expect(Math.sin(Math.PI / 2)).toBeCloseTo(1, 10);
  expect(Math.sin(Math.PI)).toBeCloseTo(0, 10);
  expect(Math.sin((3 * Math.PI) / 2)).toBeCloseTo(-1, 10);

  expect(Math.sin(NaN)).toBeNaN();
  expect(Math.sin(Infinity)).toBeNaN();
  expect(Math.sin(-Infinity)).toBeNaN();
});

test("Math.sin coerces its argument", () => {
  expect(Math.sin("0")).toBe(0);
  expect(() => Math.sin(1n)).toThrow(TypeError);
});
