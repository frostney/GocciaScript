/*---
description: Math.log10
features: [Math.log10]
---*/

describe("Math.log10", () => {
  test("returns the base-ten logarithm", () => {
    expect(Math.log10(1)).toBe(0);
    expect(Math.log10(100)).toBeCloseTo(2, 12);
    expect(Math.log10(1000)).toBeCloseTo(3, 12);
  });

  test("handles its domain boundary and special values", () => {
    expect(Math.log10(0)).toBe(-Infinity);
    expect(Math.log10(-0)).toBe(-Infinity);
    expect(Math.log10(-1)).toBeNaN();
    expect(Math.log10(Infinity)).toBe(Infinity);
    expect(Math.log10(-Infinity)).toBeNaN();
    expect(Math.log10(NaN)).toBeNaN();
  });
});
