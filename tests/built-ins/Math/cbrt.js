/*---
description: Math.cbrt
features: [Math.cbrt]
---*/

describe("Math.cbrt", () => {
  test("returns cube roots for positive and negative values", () => {
    expect(Math.cbrt(27)).toBeCloseTo(3, 12);
    expect(Math.cbrt(-8)).toBeCloseTo(-2, 12);
    expect(Math.cbrt(0.125)).toBeCloseTo(0.5, 12);
  });

  test("preserves signed zero and infinities", () => {
    expect(Math.cbrt(0)).toBe(0);
    expect(Object.is(Math.cbrt(-0), -0)).toBe(true);
    expect(Math.cbrt(Infinity)).toBe(Infinity);
    expect(Math.cbrt(-Infinity)).toBe(-Infinity);
    expect(Math.cbrt(NaN)).toBeNaN();
  });

  test("coerces its argument", () => {
    expect(Math.cbrt("27")).toBeCloseTo(3, 12);
    expect(() => Math.cbrt(8n)).toThrow(TypeError);
  });
});
