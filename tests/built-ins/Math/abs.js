/*---
description: Math.abs method returns the absolute value of a number
features: [Math.abs]
---*/

test("Math.abs", () => {
  expect(Math.abs(5)).toBe(5);
  expect(Math.abs(-5)).toBe(5);
  expect(Math.abs(0)).toBe(0);
  expect(Math.abs(-0)).toBe(0);
  expect(Math.abs(3.14)).toBe(3.14);
  expect(Math.abs(-3.14)).toBe(3.14);

  expect(Math.abs(NaN)).toBeNaN();
  expect(Math.abs(Infinity)).toBe(Infinity);
  expect(Math.abs(-Infinity)).toBe(Infinity);
});
