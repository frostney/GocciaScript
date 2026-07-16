/*---
description: Math.asin
features: [Math.asin]
---*/

describe("Math.asin", () => {
  test("returns inverse sine values at domain boundaries", () => {
    expect(Math.asin(1)).toBeCloseTo(Math.PI / 2, 12);
    expect(Math.asin(-1)).toBeCloseTo(-Math.PI / 2, 12);
    expect(Math.asin(0)).toBe(0);
    expect(Object.is(Math.asin(-0), -0)).toBe(true);
  });

  test("returns NaN outside its domain and for non-finite invalid values", () => {
    expect(Math.asin(1.0001)).toBeNaN();
    expect(Math.asin(-1.0001)).toBeNaN();
    expect(Math.asin(Infinity)).toBeNaN();
    expect(Math.asin(-Infinity)).toBeNaN();
    expect(Math.asin(NaN)).toBeNaN();
  });

  test("coerces its argument to a number", () => {
    expect(Math.asin("0")).toBe(0);
    expect(Math.asin(null)).toBe(0);
    expect(() => Math.asin(1n)).toThrow(TypeError);
  });
});
