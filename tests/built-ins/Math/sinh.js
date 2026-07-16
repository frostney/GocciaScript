/*---
description: Math.sinh
features: [Math.sinh]
---*/

describe("Math.sinh", () => {
  test("returns the hyperbolic sine", () => {
    expect(Math.sinh(1)).toBeCloseTo(1.1752011936438014, 12);
    expect(Math.sinh(-1)).toBeCloseTo(-1.1752011936438014, 12);
  });

  test("preserves signed zero and infinities", () => {
    expect(Math.sinh(0)).toBe(0);
    expect(Object.is(Math.sinh(-0), -0)).toBe(true);
    expect(Math.sinh(Infinity)).toBe(Infinity);
    expect(Math.sinh(-Infinity)).toBe(-Infinity);
    expect(Math.sinh(NaN)).toBeNaN();
  });
});
