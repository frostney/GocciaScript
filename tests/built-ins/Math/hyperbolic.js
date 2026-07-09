describe("Math hyperbolic methods", () => {
  test("Math.cosh", () => {
    expect(Math.cosh(0)).toBe(1);
    expect(Number.isNaN(Math.cosh(NaN))).toBe(true);
  });

  test("Math.sinh", () => {
    expect(Math.sinh(0)).toBe(0);
    expect(Object.is(Math.sinh(-0), -0)).toBe(true);
    expect(Number.isNaN(Math.sinh(NaN))).toBe(true);
  });

  test("Math.tanh", () => {
    expect(Math.tanh(0)).toBe(0);
    expect(Object.is(Math.tanh(-0), -0)).toBe(true);
    expect(Number.isNaN(Math.tanh(NaN))).toBe(true);
  });

  test("Math.acosh", () => {
    expect(Math.acosh(1)).toBe(0);
    expect(Math.acosh(Number.MAX_VALUE)).toBeCloseTo(710.4758600739439, 12);
    expect(Number.isNaN(Math.acosh(0.5))).toBe(true);
    expect(Number.isNaN(Math.acosh(NaN))).toBe(true);
  });

  test("Math.asinh", () => {
    expect(Math.asinh(0)).toBe(0);
    expect(Object.is(Math.asinh(-0), -0)).toBe(true);
    expect(Number.isNaN(Math.asinh(NaN))).toBe(true);
  });

  test("Math.atanh", () => {
    expect(Math.atanh(0)).toBe(0);
    expect(Object.is(Math.atanh(-0), -0)).toBe(true);
    expect(Number.isNaN(Math.atanh(NaN))).toBe(true);
  });

  test("inverse hyperbolic roundtrips stay within the expected double", () => {
    expect(Math.asinh(Math.sinh(-0.25))).toBe(-0.25);

    const loopValue = -0.24999999999999967;
    expect(Math.atanh(Math.tanh(loopValue))).toBe(loopValue);
  });
});
