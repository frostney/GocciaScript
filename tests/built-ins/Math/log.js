/*---
description: Math.log
features: [Math.log]
---*/

describe("Math.log", () => {
  test("returns the natural logarithm", () => {
    expect(Math.log(Math.E)).toBeCloseTo(1, 12);
    expect(Math.log(1)).toBe(0);
  });

  test("handles its domain boundary and special values", () => {
    expect(Math.log(0)).toBe(-Infinity);
    expect(Math.log(-0)).toBe(-Infinity);
    expect(Math.log(-1)).toBeNaN();
    expect(Math.log(Infinity)).toBe(Infinity);
    expect(Math.log(-Infinity)).toBeNaN();
    expect(Math.log(NaN)).toBeNaN();
  });
});
