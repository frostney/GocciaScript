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
