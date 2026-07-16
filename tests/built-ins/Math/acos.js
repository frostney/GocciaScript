/*---
description: Math.acos
features: [Math.acos]
---*/

describe("Math.acos", () => {
  test("returns inverse cosine values at domain boundaries", () => {
    expect(Math.acos(1)).toBe(0);
    expect(Math.acos(0)).toBeCloseTo(Math.PI / 2, 12);
    expect(Math.acos(-1)).toBeCloseTo(Math.PI, 12);
  });

  test("returns NaN outside its domain and for non-finite invalid values", () => {
    expect(Math.acos(1.0001)).toBeNaN();
    expect(Math.acos(-1.0001)).toBeNaN();
    expect(Math.acos(Infinity)).toBeNaN();
    expect(Math.acos(-Infinity)).toBeNaN();
    expect(Math.acos(NaN)).toBeNaN();
  });

  test("coerces its argument to a number", () => {
    expect(Math.acos("1")).toBe(0);
    expect(Math.acos(null)).toBeCloseTo(Math.PI / 2, 12);
    expect(() => Math.acos(1n)).toThrow(TypeError);
  });
});
