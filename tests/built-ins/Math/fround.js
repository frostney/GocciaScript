/*---
description: Math.fround
features: [Math.fround]
---*/

describe("Math.fround", () => {
  test("rounds to the nearest IEEE 754 single-precision value", () => {
    expect(Math.fround(1.337)).toBe(1.3370000123977661);
    expect(Math.fround(1.5)).toBe(1.5);
  });

  test("preserves special values", () => {
    expect(Math.fround(0)).toBe(0);
    expect(Object.is(Math.fround(-0), -0)).toBe(true);
    expect(Math.fround(Infinity)).toBe(Infinity);
    expect(Math.fround(-Infinity)).toBe(-Infinity);
    expect(Math.fround(NaN)).toBeNaN();
    expect(Math.fround()).toBeNaN();
  });
});
