describe("Math trigonometric methods", () => {
  test("Math.acos", () => {
    expect(Math.acos(1)).toBe(0);
    expect(Number.isNaN(Math.acos(2))).toBe(true);
    expect(Number.isNaN(Math.acos(NaN))).toBe(true);
  });

  test("Math.asin", () => {
    expect(Math.asin(0)).toBe(0);
    expect(Number.isNaN(Math.asin(2))).toBe(true);
    expect(Number.isNaN(Math.asin(NaN))).toBe(true);
  });

  test("Math.atan", () => {
    expect(Math.atan(0)).toBe(0);
    expect(Number.isNaN(Math.atan(NaN))).toBe(true);
  });

  test("Math.atan2", () => {
    expect(Math.atan2(0, 1)).toBe(0);
    expect(Math.atan2(1, 0)).toBe(Math.PI / 2);
    expect(Number.isNaN(Math.atan2(NaN, 1))).toBe(true);
  });
});
