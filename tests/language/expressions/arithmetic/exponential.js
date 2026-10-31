/*---
description: Exponential operator works correctly
features: [exponential-operator]
---*/

describe("exponentiation basics", () => {
  test("exponential operator", () => {
    expect(2 ** 3).toBe(8);
    expect(2 ** 0).toBe(1);
    expect(2 ** 1).toBe(2);
  });

  test("anything to the power of 0 is 1", () => {
    expect(0 ** 0).toBe(1);
    expect(Infinity ** 0).toBe(1);
    expect((-Infinity) ** 0).toBe(1);
    expect(NaN ** 0).toBe(1);
  });
});

describe("exponentiation with Infinity base", () => {
  test("Infinity ** positive is Infinity", () => {
    expect(Infinity ** 1).toBe(Infinity);
    expect(Infinity ** 2).toBe(Infinity);
    expect(Infinity ** 0.5).toBe(Infinity);
  });

  test("Infinity ** negative is 0", () => {
    expect(Infinity ** -1).toBe(0);
    expect(Infinity ** -2).toBe(0);
  });

  test("-Infinity ** positive integer", () => {
    expect((-Infinity) ** 2).toBe(Infinity);
    expect((-Infinity) ** 3).toBe(-Infinity);
    expect((-Infinity) ** 4).toBe(Infinity);
  });

  test("-Infinity ** non-integer positive is +Infinity", () => {
    expect((-Infinity) ** 0.5).toBe(Infinity);
    expect((-Infinity) ** 1.5).toBe(Infinity);
    expect((-Infinity) ** 2.5).toBe(Infinity);
  });

  test("-Infinity ** negative integer respects parity", () => {
    const negOdd = (-Infinity) ** -1;
    expect(negOdd === 0).toBe(true);
    expect(1 / negOdd).toBe(-Infinity);

    expect((-Infinity) ** -2).toBe(0);

    const negOdd3 = (-Infinity) ** -3;
    expect(negOdd3 === 0).toBe(true);
    expect(1 / negOdd3).toBe(-Infinity);
  });
});

describe("exponentiation with Infinity exponent", () => {
  test("|base| > 1 ** Infinity is Infinity", () => {
    expect(2 ** Infinity).toBe(Infinity);
    expect(10 ** Infinity).toBe(Infinity);
  });

  test("|base| > 1 ** -Infinity is 0", () => {
    expect(2 ** (-Infinity)).toBe(0);
    expect(10 ** (-Infinity)).toBe(0);
  });

  test("|base| < 1 ** Infinity is 0", () => {
    expect(0.5 ** Infinity).toBe(0);
    expect(0.1 ** Infinity).toBe(0);
  });

  test("|base| < 1 ** -Infinity is Infinity", () => {
    expect(0.5 ** (-Infinity)).toBe(Infinity);
    expect(0.1 ** (-Infinity)).toBe(Infinity);
  });

  test("|base| = 1 ** Infinity is NaN", () => {
    expect(Number.isNaN(1 ** Infinity)).toBe(true);
    expect(Number.isNaN((-1) ** Infinity)).toBe(true);
    expect(Number.isNaN(1 ** (-Infinity))).toBe(true);
    expect(Number.isNaN((-1) ** (-Infinity))).toBe(true);
  });
});

describe("exponentiation with signed zero base", () => {
  test("-0 ** positive odd integer is -0", () => {
    expect(Object.is((-0) ** 1, -0)).toBe(true);
    expect(Object.is((-0) ** 111, -0)).toBe(true);
  });

  test("-0 ** positive non-odd exponent is +0", () => {
    expect(Object.is((-0) ** 2, 0)).toBe(true);
    expect(Object.is((-0) ** Infinity, 0)).toBe(true);
  });

  test("-0 ** negative odd integer is -Infinity", () => {
    expect((-0) ** -1).toBe(-Infinity);
    expect((-0) ** -3).toBe(-Infinity);
  });

  test("-0 ** negative non-odd exponent is +Infinity", () => {
    expect((-0) ** -2).toBe(Infinity);
    expect((-0) ** (-Infinity)).toBe(Infinity);
  });
});

describe("exponentiation with NaN", () => {
  test("NaN ** non-zero is NaN", () => {
    expect(Number.isNaN(NaN ** 1)).toBe(true);
    expect(Number.isNaN(NaN ** 2)).toBe(true);
  });

  test("base ** NaN is NaN", () => {
    expect(Number.isNaN(2 ** NaN)).toBe(true);
  });
});

// Each expected value is what Node 24 returns, and is the correctly rounded
// result. `power` keeps its operands out of constant folding, and `boxed`
// takes the generic operator through valueOf.
const power = (x, y) => x ** y;
const boxed = (x, y) => ({ valueOf: () => x }) ** ({ valueOf: () => y });

describe("exponentiation of finite operands", () => {
  const cases = [
    [1e20, 0.5, 10000000000],
    [9, 0.5, 3],
    [5, 0.5, 2.23606797749979],
    [1e300, 0.5, 1e150],
    [10, -5, 0.00001],
    [10, 308, 1e308],
    [10, -308, 1e-308],
    [1.1, 30, 17.44940226888645],
    [27, 1 / 3, 3],
    [5, -3, 0.008],
    [0.1, 3, 0.0010000000000000002],
    [1.5, -7, 0.05852766346593507],
    [123.456, 7.89, 31771028258180936],
    [0.9, 1000, 1.7478712517226947e-46],
    [1.0000001, 1e7, 2.7182816941320818],
    [2, 0.1, 1.0717734625362931],
    [1e-200, 1.5, 1e-300],
    [4, -0.5, 0.5],
    [1.0000000000000002, 2 ** 52, 2.718281828459045],
    [0.9999999999999999, -(2 ** 52), 1.6487212707001282],
  ];

  test.each(cases)("%s ** %s is %s", (x, y, expected) => {
    expect(power(x, y)).toBe(expected);
    expect(boxed(x, y)).toBe(expected);
  });

  test("constant operands fold to the same results", () => {
    expect(1e20 ** 0.5).toBe(10000000000);
    expect(10 ** -5).toBe(0.00001);
    expect(1.1 ** 30).toBe(17.44940226888645);
    expect(27 ** (1 / 3)).toBe(3);
  });

  test("exactly representable results are exact", () => {
    expect(power(10, 22)).toBe(1e22);
    expect(power(10, 15)).toBe(1000000000000000);
    expect(power(3, 33)).toBe(5559060566555523);
    expect(power(-3, 33)).toBe(-5559060566555523);
    expect(power(2, 1023)).toBe(8.98846567431158e307);
    expect(power(2, -1074)).toBe(5e-324);
    expect(power(0.5, 1074)).toBe(5e-324);
    expect(power(2, -1022)).toBe(2.2250738585072014e-308);
    expect(power(2.5, 2)).toBe(6.25);
    expect(power(16, 0.25)).toBe(2);
  });

  test("** 0.5 is the square root", () => {
    for (const x of [2, 3, 5, 7, 10, 0.1, 1e-300, 5e-324, 1e308, Number.MAX_VALUE, 123456789.123]) {
      expect(power(x, 0.5)).toBe(Math.sqrt(x));
    }
  });

  test("** 2 is the product of the base with itself", () => {
    for (const x of [1.1, -3.7, 1e154, 1e-162, 0.1, 123456789.123]) {
      expect(power(x, 2)).toBe(x * x);
    }
  });

  test("results past the range overflow and underflow", () => {
    expect(power(10, 309)).toBe(Infinity);
    expect(power(-10, 309)).toBe(-Infinity);
    expect(power(2, -1075)).toBe(0);
    expect(Object.is(power(-2, -1075), -0)).toBe(true);
    expect(Object.is(power(-10, -400), 0)).toBe(true);
  });

  test("a subnormal result is rounded once", () => {
    expect(power(288, -125.09294758759437)).toBe(2.225073858507185e-308);
  });
});

describe("exponentiation with a negative base", () => {
  test("an integral exponent of 2^31 or more keeps the parity's sign", () => {
    expect(power(-2, 1e20)).toBe(Infinity);
    expect(power(-0.5, 1e20)).toBe(0);
    expect(power(-2, 2 ** 31)).toBe(Infinity);
    expect(power(-3, 2 ** 31 + 1)).toBe(-Infinity);
    expect(Object.is(power(-0.5, 2 ** 31 + 1), -0)).toBe(true);
    expect(power(-1, 1e300)).toBe(1);
    expect(power(-1, 2 ** 52 + 1)).toBe(-1);
  });

  test("an odd integral exponent gives a negative result", () => {
    expect(power(-1.1, 31)).toBe(-19.194342495775096);
    expect(power(-2, -3)).toBe(-0.125);
  });
});
