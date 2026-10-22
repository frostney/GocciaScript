/*---
description: Math.round
features: [Math.round]
---*/

describe("Math.round", () => {
  test("rounds to the nearest integer toward positive infinity on ties", () => {
    expect(Math.round(4.3)).toBe(4);
    expect(Math.round(4.7)).toBe(5);
    expect(Math.round(-4.3)).toBe(-4);
    expect(Math.round(-4.7)).toBe(-5);
    expect(Math.round(0.5)).toBe(1);
    expect(Math.round(1.5)).toBe(2);
    expect(Object.is(Math.round(-0.5), -0)).toBe(true);
    expect(Object.is(Math.round(0.5 - Number.EPSILON / 4), 0)).toBe(true);
  });

  test("rounds fractions beyond the signed 32-bit range", () => {
    for (const base of [2 ** 31, 2 ** 32, 2 ** 40]) {
      expect(Math.round(base + 0.25)).toBe(base);
      expect(Math.round(base + 0.5)).toBe(base + 1);
      expect(Math.round(base + 0.75)).toBe(base + 1);
      expect(Math.round(-base - 0.25)).toBe(-base);
      expect(Math.round(-base - 0.5)).toBe(-base);
      expect(Math.round(-base - 0.75)).toBe(-base - 1);
    }
  });

  test("rounds half ties at the limit of fractional Number values", () => {
    expect(Math.round(4503599627370495.5)).toBe(4503599627370496);
    expect(Math.round(-4503599627370495.5)).toBe(-4503599627370495);
    expect(Math.round(4503599627370494.5)).toBe(4503599627370495);
    expect(Math.round(-4503599627370494.5)).toBe(-4503599627370494);
    expect(Math.round(-0.5000000000000001)).toBe(-1);
    expect(Math.round(0.5000000000000001)).toBe(1);
  });

  test("preserves integral values across the full Number range", () => {
    for (const value of [2 ** 31, 2 ** 52 + 1, 2 ** 53 - 1, 2 ** 53,
      2 ** 53 + 2, 1e100, Number.MAX_VALUE]) {
      expect(Math.round(value)).toBe(value);
      expect(Math.round(-value)).toBe(-value);
    }
  });

  test("preserves special values and signed zero", () => {
    expect(Math.round(NaN)).toBeNaN();
    expect(Math.round(Infinity)).toBe(Infinity);
    expect(Math.round(-Infinity)).toBe(-Infinity);
    expect(Math.round(0)).toBe(0);
    expect(Object.is(Math.round(-0), -0)).toBe(true);
    expect(Object.is(Math.round(-0.25), -0)).toBe(true);
    expect(Object.is(Math.round(-Number.MIN_VALUE), -0)).toBe(true);
    expect(Object.is(Math.round(Number.MIN_VALUE), 0)).toBe(true);
  });

  test("coerces only its first argument", () => {
    expect(Math.round("1.4", { valueOf() { throw new Error("unused"); } })).toBe(1);
    expect(() => Math.round(1n)).toThrow(TypeError);
  });
});
