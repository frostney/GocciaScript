describe("Math hyperbolic methods", () => {
  test("Math.cosh", () => {
    expect(Math.cosh(0)).toBe(1);
    expect(Number.isNaN(Math.cosh(NaN))).toBe(true);
  });

  test("Math.sinh", () => {
    expect(Math.sinh(0)).toBe(0);
    expect(Number.isNaN(Math.sinh(NaN))).toBe(true);
  });

  test("Math.tanh", () => {
    expect(Math.tanh(0)).toBe(0);
    expect(Number.isNaN(Math.tanh(NaN))).toBe(true);
  });

  test("Math.acosh", () => {
    expect(Math.acosh(1)).toBe(0);
    expect(Number.isNaN(Math.acosh(0.5))).toBe(true);
    expect(Number.isNaN(Math.acosh(NaN))).toBe(true);
  });

  test("Math.asinh", () => {
    expect(Math.asinh(0)).toBe(0);
    expect(Number.isNaN(Math.asinh(NaN))).toBe(true);
  });

  test("Math.atanh", () => {
    expect(Math.atanh(0)).toBe(0);
    expect(Number.isNaN(Math.atanh(NaN))).toBe(true);
  });
});
