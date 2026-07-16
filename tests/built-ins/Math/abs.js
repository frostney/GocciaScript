/*---
description: Math.abs method returns the absolute value of a number
features: [Math.abs]
---*/

test("Math.abs", () => {
  expect(Math.abs(5)).toBe(5);
  expect(Math.abs(-5)).toBe(5);
  expect(Object.is(Math.abs(0), 0)).toBe(true);
  expect(Object.is(Math.abs(-0), 0)).toBe(true);
  expect(Math.abs(3.14)).toBe(3.14);
  expect(Math.abs(-3.14)).toBe(3.14);

  expect(Math.abs(NaN)).toBeNaN();
  expect(Math.abs(Infinity)).toBe(Infinity);
  expect(Math.abs(-Infinity)).toBe(Infinity);
});

test("Math.abs coerces its argument", () => {
  expect(Math.abs("-3.5")).toBe(3.5);
  expect(Math.abs(null)).toBe(0);
  expect(() => Math.abs(1n)).toThrow(TypeError);
});

test("Math.abs has correct name and length", () => {
  expect(Math.abs.name).toBe("abs");
  expect(Math.abs.length).toBe(1);
});
