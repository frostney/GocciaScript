describe("Math constants", () => {
  test("Math.LOG2E", () => {
    expect(typeof Math.LOG2E).toBe("number");
    expect(Math.LOG2E).toBeCloseTo(1.44269, 3);
  });

  test("Math.LOG10E", () => {
    expect(typeof Math.LOG10E).toBe("number");
    expect(Math.LOG10E).toBeCloseTo(0.43429, 3);
  });

  test("Math.SQRT1_2", () => {
    expect(typeof Math.SQRT1_2).toBe("number");
    expect(Math.SQRT1_2).toBeCloseTo(0.70710, 3);
  });

  test("Math.SQRT2", () => {
    expect(typeof Math.SQRT2).toBe("number");
    expect(Math.SQRT2).toBeCloseTo(1.41421, 3);
  });

  test("Math.LN2", () => {
    expect(typeof Math.LN2).toBe("number");
    expect(Math.LN2).toBeCloseTo(0.69314, 3);
  });

  test("Math.LN10", () => {
    expect(typeof Math.LN10).toBe("number");
    expect(Math.LN10).toBeCloseTo(2.30258, 3);
  });

  test("Math.E", () => {
    expect(typeof Math.E).toBe("number");
    expect(Math.E).toBeCloseTo(2.71828, 3);
  });

  test("Math.PI", () => {
    expect(typeof Math.PI).toBe("number");
    expect(Math.PI).toBeCloseTo(3.14159, 3);
  });
  
});
