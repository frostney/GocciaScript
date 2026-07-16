/*---
description: Math.atan
features: [Math.atan]
---*/

describe("Math.atan", () => {
  test("returns inverse tangent values", () => {
    expect(Math.atan(0)).toBe(0);
    expect(Object.is(Math.atan(-0), -0)).toBe(true);
    expect(Math.atan(1)).toBeCloseTo(Math.PI / 4, 12);
    expect(Math.atan(-1)).toBeCloseTo(-Math.PI / 4, 12);
  });

  test("handles infinities and NaN", () => {
    expect(Math.atan(Infinity)).toBeCloseTo(Math.PI / 2, 12);
    expect(Math.atan(-Infinity)).toBeCloseTo(-Math.PI / 2, 12);
    expect(Math.atan(NaN)).toBeNaN();
  });

  test("coerces its argument to a number", () => {
    expect(Math.atan("1")).toBeCloseTo(Math.PI / 4, 12);
    expect(Math.atan(null)).toBe(0);
    expect(() => Math.atan(1n)).toThrow(TypeError);
  });
});
