/*---
description: Number.prototype.toPrecision formats a number to a specified precision
features: [Number.prototype.toPrecision]
---*/

describe("Number.prototype.toPrecision", () => {
  test("toPrecision with no argument returns string representation", () => {
    expect((123.456).toPrecision()).toBe("123.456");
    expect((42).toPrecision()).toBe("42");
    expect((0).toPrecision()).toBe("0");
  });

  test("toPrecision with specified precision", () => {
    expect((5.123456).toPrecision(4)).toBe("5.123");
    expect((0.00123).toPrecision(2)).toBe("0.0012");
    expect((5).toPrecision(1)).toBe("5");
    expect((9).toPrecision(1)).toBe("9");
  });

  test("toPrecision uses deterministic binary64 rounding", () => {
    expect((1.3548387096774193).toPrecision(16)).toBe("1.354838709677419");
    expect((1 + 11 / 31).toPrecision(16)).toBe("1.354838709677419");
    expect((0).toPrecision(3)).toBe("0.00");
    expect((12345).toPrecision(3)).toBe("1.23e+4");
    expect((0.0000012345).toPrecision(3)).toBe("0.00000123");
    expect((0.00000012345).toPrecision(3)).toBe("1.23e-7");
  });

  test("toPrecision on NaN", () => {
    expect(NaN.toPrecision()).toBe("NaN");
    expect(NaN.toPrecision(3)).toBe("NaN");
  });

  test("toPrecision on Infinity", () => {
    expect(Infinity.toPrecision()).toBe("Infinity");
    expect((-Infinity).toPrecision()).toBe("-Infinity");
    expect(Infinity.toPrecision(5)).toBe("Infinity");
  });
});

describe("Number.prototype.toPrecision precision range", () => {
  test("Infinity precision throws RangeError", () => {
    expect(() => (5).toPrecision(Infinity)).toThrow(RangeError);
  });

  test("NaN this value returns NaN before the range check", () => {
    expect(NaN.toPrecision(Infinity)).toBe("NaN");
  });

  test("NaN precision counts as 0 and throws RangeError", () => {
    expect(() => (5).toPrecision(NaN)).toThrow(RangeError);
  });
});
