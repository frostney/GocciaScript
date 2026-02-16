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
