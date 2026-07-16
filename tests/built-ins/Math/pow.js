/*---
description: Math.pow
features: [Math.pow]
---*/

describe("Math.pow", () => {
  test("raises finite bases to finite exponents", () => {
    expect(Math.pow(2, 3)).toBe(8);
    expect(Math.pow(5, 2)).toBe(25);
    expect(Math.pow(4, 0.5)).toBe(2);
    expect(Math.pow(2, -3)).toBe(0.125);
    expect(Math.pow(-2, 3)).toBe(-8);
    expect(Math.pow(-2, 4)).toBe(16);
  });

  test("handles NaN and zero exponents in specification order", () => {
    expect(Math.pow(10, NaN)).toBeNaN();
    expect(Math.pow(NaN, 2)).toBeNaN();
    expect(Math.pow(NaN, 0)).toBe(1);
    expect(Math.pow(NaN, -0)).toBe(1);
  });

  test("handles positive and negative infinity bases", () => {
    expect(Math.pow(Infinity, 2)).toBe(Infinity);
    expect(Object.is(Math.pow(Infinity, -2), 0)).toBe(true);
    expect(Math.pow(-Infinity, 3)).toBe(-Infinity);
    expect(Math.pow(-Infinity, 2)).toBe(Infinity);
    expect(Object.is(Math.pow(-Infinity, -3), -0)).toBe(true);
    expect(Object.is(Math.pow(-Infinity, -2), 0)).toBe(true);
  });

  test("preserves the sign required for zero bases", () => {
    expect(Object.is(Math.pow(0, 3), 0)).toBe(true);
    expect(Math.pow(0, -3)).toBe(Infinity);
    expect(Object.is(Math.pow(-0, 3), -0)).toBe(true);
    expect(Object.is(Math.pow(-0, 2), 0)).toBe(true);
    expect(Math.pow(-0, -3)).toBe(-Infinity);
    expect(Math.pow(-0, -2)).toBe(Infinity);
  });

  test("handles infinite exponents according to the absolute base", () => {
    expect(Math.pow(2, Infinity)).toBe(Infinity);
    expect(Object.is(Math.pow(0.5, Infinity), 0)).toBe(true);
    expect(Math.pow(1, Infinity)).toBeNaN();
    expect(Math.pow(-1, Infinity)).toBeNaN();
    expect(Object.is(Math.pow(2, -Infinity), 0)).toBe(true);
    expect(Math.pow(0.5, -Infinity)).toBe(Infinity);
    expect(Math.pow(1, -Infinity)).toBeNaN();
  });

  test("returns NaN for a negative base and non-integer exponent", () => {
    expect(Math.pow(-4, 0.5)).toBeNaN();
    expect(Math.pow(-8, 1 / 3)).toBeNaN();
  });

  test("coerces base before exponent and propagates conversion failures", () => {
    const calls = [];
    const base = {
      valueOf() {
        calls.push("base");
        return 2;
      },
    };
    const exponent = {
      valueOf() {
        calls.push("exponent");
        return 3;
      },
    };

    expect(Math.pow(base, exponent)).toBe(8);
    expect(calls).toEqual(["base", "exponent"]);
    expect(() => Math.pow(1n, 2)).toThrow(TypeError);
  });

  test("has the correct name and length", () => {
    expect(Math.pow.name).toBe("pow");
    expect(Math.pow.length).toBe(2);
  });
});
