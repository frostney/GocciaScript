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
  expect(Object.is(Math.round(-0.5), -0)).toBe(true);
  expect(Object.is(Math.round(0.5 - Number.EPSILON / 4), +0)).toBe(true);
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
  expect(Object.is(Math.round(-0), -0)).toBe(true);
  expect(Object.is(Math.floor(-0), -0)).toBe(true);
  expect(Object.is(Math.ceil(-0), -0)).toBe(true);
  expect(Object.is(Math.ceil(-0.25), -0)).toBe(true);
});

test("Math rounding methods ignore extra arguments", () => {
  expect(Math.floor(1.9, "ignored", {})).toBe(1);
  expect(Math.ceil(1.1, "ignored", {})).toBe(2);
  expect(Math.round(1.4, "ignored", {})).toBe(1);
});
