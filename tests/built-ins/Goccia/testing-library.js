describe("toBeCloseTo non-finite precision", () => {
  test("Infinity precision means zero tolerance", () => {
    expect(0.1).not.toBeCloseTo(0.2, Infinity);
  });

  test("NaN precision counts as 0", () => {
    expect(0.1).toBeCloseTo(0.2, NaN);
  });
});

describe("test.each title formatting with non-finite numbers", () => {
  test.each([[Infinity], [-Infinity], [NaN]])("renders %d without crashing", (value) => {
    expect(typeof value).toBe("number");
  });
});

describe("toBeCloseTo -Infinity precision", () => {
  test("-Infinity precision means infinite tolerance", () => {
    expect(0.1).toBeCloseTo(99999, -Infinity);
  });
});

describe("testing library arity validation", () => {
  test("matchers reject extra arguments", () => {
    expect(() => expect(1).toBe(1, "unexpected")).toThrow();
  });

  test("zero-argument matchers reject extra arguments", () => {
    const fn = mock();
    fn();

    expect(() => expect(fn).toHaveBeenCalled("unexpected")).toThrow();
  });
});
