/*---
description: Math.acosh
features: [Math.acosh]
---*/

describe("Math.acosh", () => {
  test("returns the inverse hyperbolic cosine within its domain", () => {
    expect(Math.acosh(1)).toBe(0);
    expect(Math.acosh(Number.MAX_VALUE)).toBeCloseTo(710.4758600739439, 12);
  });

  test("handles values outside its domain, infinities, and NaN", () => {
    expect(Math.acosh(0.5)).toBeNaN();
    expect(Math.acosh(-Infinity)).toBeNaN();
    expect(Math.acosh(Infinity)).toBe(Infinity);
    expect(Math.acosh(NaN)).toBeNaN();
  });
});
