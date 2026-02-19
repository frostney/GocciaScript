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

describe("exponentiation with NaN", () => {
  test("NaN ** non-zero is NaN", () => {
    expect(Number.isNaN(NaN ** 1)).toBe(true);
    expect(Number.isNaN(NaN ** 2)).toBe(true);
  });

  test("base ** NaN is NaN", () => {
    expect(Number.isNaN(2 ** NaN)).toBe(true);
  });
});
