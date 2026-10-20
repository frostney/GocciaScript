/*---
description: Math.trunc
features: [Math.trunc]
---*/

test("Math.trunc", () => {
  expect(Math.trunc(1.3)).toBe(1);
  expect(Math.trunc(-1.3)).toBe(-1);
  expect(Math.trunc(0.3)).toBeCloseTo(0);
  expect(Math.trunc(2.8)).toBe(2);
  expect(Math.trunc(-2.8)).toBe(-2);
  expect(Math.trunc(42.4452232115)).toBe(42);
  expect(Math.trunc(NaN)).toBeNaN();
  expect(Math.trunc(Infinity)).toBe(Infinity);
  expect(Math.trunc(-Infinity)).toBe(-Infinity);
  expect(Object.is(Math.trunc(-0), -0)).toBe(true);
  expect(Object.is(Math.trunc(-0.25), -0)).toBe(true);
  expect(Math.trunc(Number.MAX_VALUE)).toBe(Number.MAX_VALUE);
});

test("Math.trunc retains Number range beyond 32-bit integers", () => {
  for (const base of [2 ** 31, 2 ** 32, 2 ** 40]) {
    expect(Math.trunc(base + 0.75)).toBe(base);
    expect(Math.trunc(-base - 0.75)).toBe(-base);
  }
  for (const value of [2 ** 52 + 1, 2 ** 53 + 2, Number.MAX_VALUE]) {
    expect(Math.trunc(value)).toBe(value);
    expect(Math.trunc(-value)).toBe(-value);
  }
});
