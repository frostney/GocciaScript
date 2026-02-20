describe("Number.prototype.toExponential", () => {
  test("basic exponential notation", () => {
    const result = (123456).toExponential(2);
    expect(result).toBe("1.23e+5");
  });

  test("zero fraction digits", () => {
    const result = (123).toExponential(0);
    expect(result).toBe("1e+2");
  });

  test("NaN returns 'NaN'", () => {
    expect(NaN.toExponential()).toBe("NaN");
  });

  test("Infinity returns 'Infinity'", () => {
    expect(Infinity.toExponential()).toBe("Infinity");
  });

  test("negative Infinity returns '-Infinity'", () => {
    expect((-Infinity).toExponential()).toBe("-Infinity");
  });

  test("negative number", () => {
    const result = (-1.23).toExponential(2);
    expect(result.startsWith("-")).toBe(true);
  });

  test("zero with fraction digits", () => {
    const result = (0).toExponential(2);
    expect(result).toBe("0.00e+0");
  });

  test("throws RangeError for negative fraction digits", () => {
    expect(() => (1).toExponential(-1)).toThrow(RangeError);
  });

  test("throws RangeError for fraction digits > 100", () => {
    expect(() => (1).toExponential(101)).toThrow(RangeError);
  });
});
