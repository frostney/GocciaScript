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
    expect(Math.clamp(3, 5, 5)).toBe(5);
    expect(Math.clamp(7, 5, 5)).toBe(5);
    expect(Math.clamp(5, 5, 5)).toBe(5);
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

describe.runIf(hasMathClamp && typeof Goccia !== "undefined")("Math.clamp across a collection", () => {
  // Reuses the memory a wrongly collected number leaves behind.
  const churn = () => Array.from({ length: 4000 }, (_, i) => i + 0.5);
  const fresh = (value) => ({ valueOf: () => [value][0] * 1 });
  const collecting = (value) => ({
    valueOf() {
      Goccia.gc();
      churn();
      return value;
    },
  });

  test("keeps the coerced value and lower while upper's valueOf collects", () => {
    expect(Math.clamp(fresh(5.5), fresh(0.5), collecting(10))).toBe(5.5);
    expect(Math.clamp(fresh(-5), fresh(0.5), collecting(10))).toBe(0.5);
  });
});
