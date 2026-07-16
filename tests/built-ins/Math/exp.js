/*---
description: Math.exp
features: [Math.exp]
---*/

describe("Math.exp", () => {
  test("returns e raised to its argument", () => {
    expect(Math.exp(0)).toBe(1);
    expect(Math.exp(1)).toBeCloseTo(Math.E, 12);
    expect(Math.exp(-1)).toBeCloseTo(1 / Math.E, 12);
  });

  test("handles infinities and NaN", () => {
    expect(Math.exp(Infinity)).toBe(Infinity);
    expect(Math.exp(-Infinity)).toBe(0);
    expect(Math.exp(NaN)).toBeNaN();
  });
});
