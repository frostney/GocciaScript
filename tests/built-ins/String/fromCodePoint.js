describe("String.fromCodePoint", () => {
  test("ASCII code points", () => {
    expect(String.fromCodePoint(65)).toBe("A");
    expect(String.fromCodePoint(90)).toBe("Z");
  });

  test("multiple code points", () => {
    expect(String.fromCodePoint(72, 101, 108, 108, 111)).toBe("Hello");
  });

  test("apply with dense numeric array", () => {
    expect(String.fromCodePoint.apply(null, [65, 66, 67])).toBe("ABC");
  });

  test("apply falls back for non-number elements", () => {
    let converted = false;
    const codePoint = {
      valueOf() {
        converted = true;
        return 68;
      },
    };

    expect(String.fromCodePoint.apply(null, [codePoint])).toBe("D");
    expect(converted).toBe(true);
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
