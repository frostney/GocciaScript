describe("btoa", () => {
  test("typeof btoa is function", () => {
    expect(typeof btoa).toBe("function");
  });

  test("encodes empty string", () => {
    expect(btoa("")).toBe("");
  });

  test("encodes basic ASCII strings", () => {
    expect(btoa("Hello")).toBe("SGVsbG8=");
    expect(btoa("Hello, World!")).toBe("SGVsbG8sIFdvcmxkIQ==");
    expect(btoa("abc")).toBe("YWJj");
    expect(btoa("ab")).toBe("YWI=");
    expect(btoa("a")).toBe("YQ==");
  });

  test("encodes all printable ASCII characters", () => {
    expect(btoa(" ")).toBe("IA==");
    expect(btoa("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")).toBe(
      "QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVphYmNkZWZnaGlqa2xtbm9wcXJzdHV2d3h5ejAxMjM0NTY3ODk="
    );
  });

  test("encodes Latin-1 characters (code points 0x80-0xFF)", () => {
    // \xC0 = 192 (À), \xFF = 255 (ÿ)
    expect(btoa(String.fromCharCode(0xC0))).toBe("wA==");
    expect(btoa(String.fromCharCode(0xFF))).toBe("/w==");
    expect(btoa(String.fromCharCode(0x80))).toBe("gA==");
    expect(btoa(String.fromCharCode(0xA9))).toBe("qQ==");
  });

  test("encodes null bytes", () => {
    expect(btoa(String.fromCharCode(0))).toBe("AA==");
    expect(btoa(String.fromCharCode(0, 0, 0))).toBe("AAAA");
  });

  test("throws InvalidCharacterError for characters > U+00FF", () => {
    const inputs = [
      String.fromCharCode(0x0100), // U+0100 (Latin Extended-A)
      "中",                         // U+4E2D (Chinese character)
      "abc" + String.fromCharCode(0x100), // Mixed: ASCII followed by out-of-range
    ];
    inputs.forEach((input) => {
      try {
        btoa(input);
        expect(true).toBe(false); // should not reach here
      } catch (e) {
        expect(e.name).toBe("InvalidCharacterError");
        expect(e.code).toBe(5);
      }
    });
  });

  test("throws TypeError when called with no arguments", () => {
    expect(() => btoa()).toThrow();
  });

  test("coerces non-string arguments to string", () => {
    expect(btoa(123)).toBe("MTIz");
    expect(btoa(true)).toBe("dHJ1ZQ==");
    expect(btoa(null)).toBe("bnVsbA==");
    expect(btoa(undefined)).toBe("dW5kZWZpbmVk");
  });

  test("handles padding correctly for various input lengths", () => {
    // Length 1 → 4 output chars with ==
    expect(btoa("f")).toBe("Zg==");
    // Length 2 → 4 output chars with =
    expect(btoa("fo")).toBe("Zm8=");
    // Length 3 → 4 output chars, no padding
    expect(btoa("foo")).toBe("Zm9v");
    // Length 4 → 8 output chars with ==
    expect(btoa("foob")).toBe("Zm9vYg==");
    // Length 5 → 8 output chars with =
    expect(btoa("fooba")).toBe("Zm9vYmE=");
    // Length 6 → 8 output chars, no padding
    expect(btoa("foobar")).toBe("Zm9vYmFy");
  });

  test("round-trips with atob", () => {
    const inputs = ["", "Hello", "Hello, World!", "abc", "a", "ab"];
    inputs.forEach((input) => {
      expect(atob(btoa(input))).toBe(input);
    });
  });

  test("round-trips Latin-1 with atob", () => {
    const input = String.fromCharCode(0x00, 0x7F, 0x80, 0xFF);
    expect(atob(btoa(input))).toBe(input);
  });
});
