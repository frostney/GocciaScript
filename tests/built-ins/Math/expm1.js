/*---
description: Math.expm1
features: [Math.expm1]
---*/

describe("Math.expm1", () => {
  test("returns e raised to a value minus one", () => {
    expect(Math.expm1(1)).toBeCloseTo(Math.E - 1, 12);
    expect(Math.expm1(-1)).toBeCloseTo(1 / Math.E - 1, 12);
  });

  test("handles zeros, infinities, and NaN", () => {
    expect(Math.expm1(0)).toBe(0);
    expect(Object.is(Math.expm1(-0), -0)).toBe(true);
    expect(Math.expm1(Infinity)).toBe(Infinity);
    expect(Math.expm1(-Infinity)).toBe(-1);
    expect(Math.expm1(NaN)).toBeNaN();
  });
});
