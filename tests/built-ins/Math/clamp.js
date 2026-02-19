/*---
description: Math.clamp
features: [Math.clamp]
---*/

const hasMathClamp = typeof Math.clamp === "function";

describe.runIf(hasMathClamp)("Math.clamp", () => {
  test("clamps value within range", () => {
    expect(Math.clamp(5, 0, 10)).toBe(5);
    expect(Math.clamp(-5, 0, 10)).toBe(0);
    expect(Math.clamp(15, 0, 10)).toBe(10);
  });

  test("supports Infinities", () => {
    expect(Math.clamp(5, 0, Infinity)).toBe(5);
    expect(Math.clamp(-5, -Infinity, 10)).toBe(-5);
    expect(Math.clamp(5, 0, Infinity)).toBe(Math.max(5, 0));
    expect(Math.clamp(-5, -Infinity, 10)).toBe(Math.min(-5, 10));
  });

  test("returns NaN when any argument is NaN", () => {
    expect(Math.clamp(NaN, 0, 10)).toBeNaN();
    expect(Math.clamp(5, NaN, 10)).toBeNaN();
    expect(Math.clamp(5, 0, NaN)).toBeNaN();
  });

  test("supports negative zero", () => {
    expect(Math.clamp(-2, -0, 10)).toBeCloseTo(0);
    expect(Math.clamp(-0, -0, 10)).toBeCloseTo(0);
    expect(Math.clamp(0, -0, 10)).toBeCloseTo(0);
  });

  test("throws RangeError if min > max", () => {
    expect(() => Math.clamp(10, 5, 0)).toThrow(RangeError);
  });
});
