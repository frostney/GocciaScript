/*---
description: Number.prototype.toFixed correctly formats numbers with fixed-point notation
features: [Number.prototype.toFixed]
---*/

describe("Number.prototype.toFixed", () => {
  test("toFixed with no arguments defaults to 0 decimal places", () => {
    expect((3.14159).toFixed()).toBe("3");
    expect((0).toFixed()).toBe("0");
    expect((100).toFixed()).toBe("100");
  });

  test("toFixed with specified decimal places", () => {
    expect((6.5).toFixed(2)).toBe("6.50");
    expect((3.14159).toFixed(2)).toBe("3.14");
    expect((1.5).toFixed(0)).toBe("2");
    expect((1.4).toFixed(0)).toBe("1");
    expect((1).toFixed(5)).toBe("1.00000");
    expect((0).toFixed(3)).toBe("0.000");
  });

  test("toFixed on negative numbers", () => {
    expect((-3.14).toFixed(1)).toBe("-3.1");
    expect((-0.5).toFixed(0)).toBe("-1");
    expect((-100).toFixed(2)).toBe("-100.00");
  });

  test("toFixed on zero and negative zero", () => {
    expect((0).toFixed(2)).toBe("0.00");
    expect((-0).toFixed(2)).toBe("0.00");
  });

  test("toFixed on NaN returns NaN", () => {
    expect(NaN.toFixed(2)).toBe("NaN");
    expect(NaN.toFixed(0)).toBe("NaN");
    expect(NaN.toFixed()).toBe("NaN");
  });

  test("toFixed on Infinity returns Infinity", () => {
    expect(Infinity.toFixed(2)).toBe("Infinity");
    expect((-Infinity).toFixed(2)).toBe("-Infinity");
    expect(Infinity.toFixed()).toBe("Infinity");
  });

  test("toFixed used in template literal", () => {
    const total = 6.5;
    expect(`$${total.toFixed(2)}`).toBe("$6.50");
  });
});
