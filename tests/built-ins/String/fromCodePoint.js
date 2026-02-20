describe("String.fromCodePoint", () => {
  test("ASCII code points", () => {
    expect(String.fromCodePoint(65)).toBe("A");
    expect(String.fromCodePoint(90)).toBe("Z");
  });

  test("multiple code points", () => {
    expect(String.fromCodePoint(72, 101, 108, 108, 111)).toBe("Hello");
  });

  test("no arguments returns empty string", () => {
    expect(String.fromCodePoint()).toBe("");
  });

  test("invalid code point throws RangeError", () => {
    expect(() => String.fromCodePoint(-1)).toThrow(RangeError);
    expect(() => String.fromCodePoint(1.5)).toThrow(RangeError);
    expect(() => String.fromCodePoint(Infinity)).toThrow(RangeError);
  });
});
