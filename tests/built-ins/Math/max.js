/*---
description: Math.max
features: [Math.max]
---*/

test("Math.max", () => {
  expect(Math.max(1, 2, 3)).toBe(3);
  expect(Math.max(-1, -2, -3)).toBe(-1);
  expect(Math.max(1, 2, 3)).toBe(3);
  expect(Math.max(1, 2, 3, 4, 5)).toBe(5);
  expect(Math.max(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)).toBe(10);

  expect(Math.max()).toBe(-Infinity);

  expect(Math.max(NaN, 1, 2)).toBeNaN();
  expect(Math.max(1, 2, NaN)).toBeNaN();
  expect(Math.max(NaN, NaN)).toBeNaN();
  expect(Math.max(Infinity, 1, 2)).toBe(Infinity);
  expect(Math.max(-Infinity, 1, 2)).toBe(2);
  expect(Math.max(Infinity, -Infinity)).toBe(Infinity);
});

test("Math.max coerces all arguments before returning NaN", () => {
  let calls = 0;
  const coercible = {
    valueOf: () => {
      calls += 1;
      return 1;
    },
  };

  expect(Math.max(NaN, coercible)).toBeNaN();
  expect(calls).toBe(1);
});

test("Math.max treats positive zero as greater than negative zero", () => {
  expect(Object.is(Math.max(-0, +0), +0)).toBe(true);
  expect(Object.is(Math.max(+0, -0), +0)).toBe(true);
});

test("Math.max has correct name and length", () => {
  expect(Math.max.name).toBe("max");
  expect(Math.max.length).toBe(2);
  const desc = Object.getOwnPropertyDescriptor(Math.max, "length");
  expect(desc.configurable).toBe(true);
  expect(desc.enumerable).toBe(false);
  expect(desc.writable).toBe(false);
});
