describe("Math.f16round", () => {
  test("Math.f16round is a function", () => {
    expect(typeof Math.f16round).toBe("function");
  });

  test("returns NaN for NaN input", () => {
    expect(Number.isNaN(Math.f16round(NaN))).toBe(true);
  });

  test("returns Infinity for Infinity", () => {
    expect(Math.f16round(Infinity)).toBe(Infinity);
  });

  test("returns -Infinity for -Infinity", () => {
    expect(Math.f16round(-Infinity)).toBe(-Infinity);
  });

  test("returns 0 for 0", () => {
    expect(Math.f16round(0)).toBe(0);
  });

  test("returns -0 for -0", () => {
    expect(Object.is(Math.f16round(-0), -0)).toBe(true);
  });

  test("rounds to nearest float16", () => {
    // 1.337 rounds to the nearest half-precision value
    expect(Math.f16round(1.337)).toBeCloseTo(1.3369140625, 10);
  });

  test("exact float16 values are preserved", () => {
    expect(Math.f16round(1.0)).toBe(1.0);
    expect(Math.f16round(1.5)).toBe(1.5);
    expect(Math.f16round(2.0)).toBe(2.0);
    expect(Math.f16round(0.5)).toBe(0.5);
  });

  test("max finite float16 is 65504", () => {
    expect(Math.f16round(65504)).toBe(65504);
  });

  test("values exceeding max overflow to Infinity", () => {
    expect(Math.f16round(65520)).toBe(Infinity);
  });

  test("negative values", () => {
    expect(Math.f16round(-1.5)).toBe(-1.5);
    expect(Math.f16round(-65504)).toBe(-65504);
    expect(Math.f16round(-65520)).toBe(-Infinity);
  });

  test("small subnormals", () => {
    // Minimum positive subnormal: 2^(-24)
    const minSubnormal = 5.960464477539063e-8;
    expect(Math.f16round(minSubnormal)).toBeCloseTo(minSubnormal, 15);
  });

  test("values smaller than smallest subnormal round to zero", () => {
    expect(Math.f16round(1e-10)).toBe(0);
  });

  test("round to nearest even on tie", () => {
    // 1.0 + 2^(-11) = 1.00048828125 ties between 1.0 and 1.0009765625
    // mantissa of 1.0 is 0 (even), so rounds down to 1.0
    expect(Math.f16round(1.00048828125)).toBe(1);
  });
});
