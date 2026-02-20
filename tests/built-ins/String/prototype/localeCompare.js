describe("String.prototype.localeCompare", () => {
  test("returns 0 for equal strings", () => {
    expect("abc".localeCompare("abc")).toBe(0);
  });

  test("returns negative for string that sorts before", () => {
    expect("a".localeCompare("b")).toBe(-1);
  });

  test("returns positive for string that sorts after", () => {
    expect("b".localeCompare("a")).toBe(1);
  });

  test("empty strings are equal", () => {
    expect("".localeCompare("")).toBe(0);
  });

  test("empty string sorts before non-empty", () => {
    expect("".localeCompare("a")).toBe(-1);
  });

  test("case-sensitive comparison", () => {
    const result = "A".localeCompare("a");
    expect(result !== 0).toBe(true);
  });
});
