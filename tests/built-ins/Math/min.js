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

describe.runIf(typeof Goccia !== "undefined")("Math.min across a collection", () => {
  // Reuses the memory a wrongly collected number leaves behind.
  const churn = () => Array.from({ length: 4000 }, (_, i) => i + 0.5);
  const fresh = (value) => ({ valueOf: () => [value][0] * 1 });
  const collecting = (value) => ({
    valueOf() {
      Goccia.gc();
      churn();
      return value;
    },
  });

  test("keeps an earlier coerced argument while a later valueOf collects", () => {
    expect(Math.min(fresh(-9.5), collecting(1))).toBe(-9.5);
  });
});
