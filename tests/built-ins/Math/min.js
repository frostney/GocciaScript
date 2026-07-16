/*---
description: Math.min
features: [Math.min]
---*/

test("Math.min", () => {
  expect(Math.min(1, 2, 3)).toBe(1);
  expect(Math.min(-1, -2, -3)).toBe(-3);

  expect(Math.min()).toBe(Infinity);

  expect(Math.min(NaN, 1, 2)).toBeNaN();
  expect(Math.min(1, 2, NaN)).toBeNaN();
  expect(Math.min(NaN, NaN)).toBeNaN();
  expect(Math.min(Infinity, 1, 2)).toBe(1);
  expect(Math.min(-Infinity, 1, 2)).toBe(-Infinity);
  expect(Math.min(Infinity, -Infinity)).toBe(-Infinity);
  expect(Math.min(-Infinity, Infinity)).toBe(-Infinity);
});

test("Math.min coerces all arguments before returning NaN", () => {
  let calls = 0;
  const coercible = {
    valueOf: () => {
      calls += 1;
      return 1;
    },
  };

  expect(Math.min(NaN, coercible)).toBeNaN();
  expect(calls).toBe(1);
});

test("Math.min treats negative zero as less than positive zero", () => {
  expect(Object.is(Math.min(+0, -0), -0)).toBe(true);
  expect(Object.is(Math.min(-0, +0), -0)).toBe(true);
});

test("Math.min has correct name and length", () => {
  expect(Math.min.name).toBe("min");
  expect(Math.min.length).toBe(2);
});
