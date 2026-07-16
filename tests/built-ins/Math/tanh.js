/*---
description: Math.tanh
features: [Math.tanh]
---*/

describe("Math.tanh", () => {
  test("returns the hyperbolic tangent", () => {
    expect(Math.tanh(1)).toBeCloseTo(0.7615941559557649, 12);
    expect(Math.tanh(-1)).toBeCloseTo(-0.7615941559557649, 12);
  });

  test("preserves signed zero and maps infinities to signed one", () => {
    expect(Math.tanh(0)).toBe(0);
    expect(Object.is(Math.tanh(-0), -0)).toBe(true);
    expect(Math.tanh(Infinity)).toBe(1);
    expect(Math.tanh(-Infinity)).toBe(-1);
    expect(Math.tanh(NaN)).toBeNaN();
  });
});
