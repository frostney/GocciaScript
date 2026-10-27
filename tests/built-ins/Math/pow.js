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

describe.runIf(typeof Goccia !== "undefined")("Math.pow across a collection", () => {
  // Reuses the memory a wrongly collected number leaves behind.
  const churn = () => Array.from({ length: 4000 }, (_, i) => i + 0.5);

  test("keeps the coerced base while the exponent's valueOf collects", () => {
    const base = { valueOf: () => [1, 2].length };
    const exponent = {
      valueOf() {
        Goccia.gc();
        churn();
        return 3;
      },
    };
    expect(Math.pow(base, exponent)).toBe(8);
  });
});

describe("Math.pow precision", () => {
  test("returns the same Number as the ** operator", () => {
    const bases = [1e20, 9, 10, 1.1, 27, -3, -2, 0.9, 1.0000001, 5e-324, 1e308];
    const exponents = [0.5, 2, -5, 30, 1 / 3, 33, 2 ** 31, 1000, 1e7, -0.5, 7.89];
    for (const x of bases) {
      for (const y of exponents) {
        expect(Object.is(Math.pow(x, y), x ** y)).toBe(true);
      }
    }
  });

  test("is correctly rounded where a power and logarithm lose precision", () => {
    expect(Math.pow(1e20, 0.5)).toBe(10000000000);
    expect(Math.pow(10, -5)).toBe(0.00001);
    expect(Math.pow(10, 308)).toBe(1e308);
    expect(Math.pow(1.1, 30)).toBe(17.44940226888645);
    expect(Math.pow(5, -3)).toBe(0.008);
  });

  test("keeps the sign of a negative base for large integral exponents", () => {
    expect(Math.pow(-2, 2 ** 31)).toBe(Infinity);
    expect(Math.pow(-3, 2 ** 31 + 1)).toBe(-Infinity);
    expect(Math.pow(-1, 1e300)).toBe(1);
  });
});
