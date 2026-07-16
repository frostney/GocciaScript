/*---
description: Math.asinh
features: [Math.asinh]
---*/

describe("Math.asinh", () => {
  test("returns the inverse hyperbolic sine", () => {
    expect(Math.asinh(Math.sinh(-0.25))).toBeCloseTo(-0.25, 15);
    expect(Math.asinh(1)).toBeCloseTo(0.881373587019543, 12);
  });

  test("preserves signed zero and infinities", () => {
    expect(Math.asinh(0)).toBe(0);
    expect(Object.is(Math.asinh(-0), -0)).toBe(true);
    expect(Math.asinh(Infinity)).toBe(Infinity);
    expect(Math.asinh(-Infinity)).toBe(-Infinity);
    expect(Math.asinh(NaN)).toBeNaN();
  });
});
