/*---
description: Math.log1p
features: [Math.log1p]
---*/

describe("Math.log1p", () => {
  test("returns the natural logarithm of one plus its argument", () => {
    expect(Math.log1p(Math.E - 1)).toBeCloseTo(1, 12);
    expect(Math.log1p(1)).toBeCloseTo(Math.LN2, 12);
  });

  test("handles its domain boundary and special values", () => {
    expect(Math.log1p(0)).toBe(0);
    expect(Object.is(Math.log1p(-0), -0)).toBe(true);
    expect(Math.log1p(-1)).toBe(-Infinity);
    expect(Math.log1p(-1.1)).toBeNaN();
    expect(Math.log1p(Infinity)).toBe(Infinity);
    expect(Math.log1p(-Infinity)).toBeNaN();
    expect(Math.log1p(NaN)).toBeNaN();
  });
});
