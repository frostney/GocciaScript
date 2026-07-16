/*---
description: Math.atanh
features: [Math.atanh]
---*/

describe("Math.atanh", () => {
  test("returns known inverse hyperbolic tangent values within its domain", () => {
    expect(Math.atanh(0.5)).toBeCloseTo(0.5493061443340548, 15);
    expect(Math.atanh(-0.5)).toBeCloseTo(-0.5493061443340548, 15);
  });

  test("handles domain boundaries and out-of-domain values", () => {
    expect(Math.atanh(1)).toBe(Infinity);
    expect(Math.atanh(-1)).toBe(-Infinity);
    expect(Math.atanh(1.1)).toBeNaN();
    expect(Math.atanh(-1.1)).toBeNaN();
    expect(Math.atanh(Infinity)).toBeNaN();
    expect(Math.atanh(NaN)).toBeNaN();
  });

  test("preserves signed zero", () => {
    expect(Math.atanh(0)).toBe(0);
    expect(Object.is(Math.atanh(-0), -0)).toBe(true);
  });
});
