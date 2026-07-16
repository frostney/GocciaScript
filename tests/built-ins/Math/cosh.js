/*---
description: Math.cosh
features: [Math.cosh]
---*/

describe("Math.cosh", () => {
  test("returns the hyperbolic cosine", () => {
    expect(Math.cosh(0)).toBe(1);
    expect(Math.cosh(1)).toBeCloseTo(1.5430806348152437, 12);
    expect(Math.cosh(-1)).toBeCloseTo(Math.cosh(1), 12);
  });

  test("handles infinities and NaN", () => {
    expect(Math.cosh(Infinity)).toBe(Infinity);
    expect(Math.cosh(-Infinity)).toBe(Infinity);
    expect(Math.cosh(NaN)).toBeNaN();
  });
});
