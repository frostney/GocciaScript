/*---
description: Math.ceil
features: [Math.ceil]
---*/

describe("Math.ceil", () => {
  test("returns the least integer greater than or equal to its argument", () => {
    expect(Math.ceil(4.3)).toBe(5);
    expect(Math.ceil(4.7)).toBe(5);
    expect(Math.ceil(-4.3)).toBe(-4);
    expect(Math.ceil(-4.7)).toBe(-4);
  });

  test("preserves special values and signed zero", () => {
    expect(Math.ceil(NaN)).toBeNaN();
    expect(Math.ceil(Infinity)).toBe(Infinity);
    expect(Math.ceil(-Infinity)).toBe(-Infinity);
    expect(Math.ceil(0)).toBe(0);
    expect(Object.is(Math.ceil(-0), -0)).toBe(true);
    expect(Object.is(Math.ceil(-0.25), -0)).toBe(true);
  });

  test("returns Number values across the 32-bit integer boundary without narrowing", () => {
    // ES2026 §21.3.2.10: return the smallest integral Number not less than x.
    expect(Math.ceil(2147483647.5)).toBe(2147483648);
    expect(Math.ceil(4294967295 / 2)).toBe(2147483648);
    expect(Math.ceil(-(4294967295 / 2))).toBe(-2147483647);
  });

  test("coerces only its first argument", () => {
    expect(Math.ceil("1.1", { valueOf() { throw new Error("unused"); } })).toBe(2);
    expect(() => Math.ceil(1n)).toThrow(TypeError);
  });
});
