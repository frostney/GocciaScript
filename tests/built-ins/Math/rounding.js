/*---
description: Math rounding methods (round, floor, ceil) work correctly
features: [Math.round, Math.floor, Math.ceil]
---*/

test("Math.round, Math.floor, Math.ceil", () => {
  expect(Math.round(4.3)).toBe(4);
  expect(Math.round(4.7)).toBe(5);
  expect(Math.round(-4.3)).toBe(-4);
  expect(Math.round(-4.7)).toBe(-5);

  expect(Math.floor(4.3)).toBe(4);
  expect(Math.floor(4.7)).toBe(4);
  expect(Math.floor(-4.3)).toBe(-5);
  expect(Math.floor(-4.7)).toBe(-5);

  expect(Math.ceil(4.3)).toBe(5);
  expect(Math.ceil(4.7)).toBe(5);
  expect(Math.ceil(-4.3)).toBe(-4);
  expect(Math.ceil(-4.7)).toBe(-4);
});

test("Math.round tie-breaking rounds up per spec", () => {
  expect(Math.round(0.5)).toBe(1);
  expect(Math.round(1.5)).toBe(2);
  expect(Math.round(-0.5)).toBe(-0);
});

test("Math rounding with NaN and Infinity", () => {
  expect(Math.round(NaN)).toBeNaN();
  expect(Math.floor(NaN)).toBeNaN();
  expect(Math.ceil(NaN)).toBeNaN();
  expect(Math.round(Infinity)).toBe(Infinity);
  expect(Math.floor(-Infinity)).toBe(-Infinity);
  expect(Math.ceil(Infinity)).toBe(Infinity);
});

test("Math rounding with zero", () => {
  expect(Math.round(0)).toBe(0);
  expect(Math.floor(0)).toBe(0);
  expect(Math.ceil(0)).toBe(0);
});
