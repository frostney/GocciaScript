describe("Number.prototype.toExponential", () => {
  test("basic exponential notation", () => {
    const result = (123456).toExponential(2);
    expect(result).toBe("1.23e+5");
  });

  test("zero fraction digits", () => {
    const result = (123).toExponential(0);
    expect(result).toBe("1e+2");
  });

  test("undefined fraction digits use the shortest exponential representation", () => {
    expect((123.456).toExponential(undefined)).toBe("1.23456e+2");
    expect((1.1e-32).toExponential()).toBe("1.1e-32");
    expect((100).toExponential(undefined)).toBe("1e+2");
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

  test("rounds explicit fraction digits from the binary64 value", () => {
    expect((123.456).toExponential(17)).toBe("1.23456000000000003e+2");
    expect((0.0001).toExponential(20)).toBe("1.00000000000000004792e-4");
    expect((0.9999).toExponential(20)).toBe("9.99900000000000011013e-1");
  });

  test("throws RangeError for negative fraction digits", () => {
    expect(() => (1).toExponential(-1)).toThrow(RangeError);
  });

  test("throws RangeError for fraction digits > 100", () => {
    expect(() => (1).toExponential(101)).toThrow(RangeError);
  });
});

describe("Number.prototype.toExponential non-finite digits", () => {
  test("Infinity digits throws RangeError", () => {
    expect(() => (5).toExponential(Infinity)).toThrow(RangeError);
  });

  test("-Infinity digits throws RangeError", () => {
    expect(() => (5).toExponential(-Infinity)).toThrow(RangeError);
  });

  test("non-finite receivers still coerce explicit fraction digits first", () => {
    expect(NaN.toExponential(Infinity)).toBe("NaN");
    expect(Infinity.toExponential(1000)).toBe("Infinity");
    expect(() => NaN.toExponential(Symbol("digits"))).toThrow(TypeError);
    expect(() => NaN.toExponential({
      valueOf() {
        throw new Error("digits");
      },
    })).toThrow(Error);
  });
});
