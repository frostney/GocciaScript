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

  test("uses Intl.Collator options", () => {
    expect("A".localeCompare("a", "en", { sensitivity: "base" })).toBe(0);
    expect("2".localeCompare("10", "en", { numeric: true }) < 0).toBe(true);
  });

  test("object-coerces non-undefined options", () => {
    expect(() => "a".localeCompare("b", "en", null)).toThrow(TypeError);
    expect(typeof "a".localeCompare("b", "en", true)).toBe("number");
  });

  test("rejects invalid locale arguments", () => {
    expect(() => "a".localeCompare("b", null)).toThrow(TypeError);
    expect(() => "a".localeCompare("b", [NaN])).toThrow(TypeError);
    expect(() => "a".localeCompare("b", ["i"])).toThrow(RangeError);
    expect(() => "a".localeCompare("b", ["de_DE"])).toThrow(RangeError);
  });
});
