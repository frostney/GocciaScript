describe("atob", () => {
  test("typeof atob is function", () => {
    expect(typeof atob).toBe("function");
  });

  test("decodes empty string", () => {
    expect(atob("")).toBe("");
  });

  test("decodes basic ASCII base64 strings", () => {
    expect(atob("SGVsbG8=")).toBe("Hello");
    expect(atob("SGVsbG8sIFdvcmxkIQ==")).toBe("Hello, World!");
    expect(atob("YWJj")).toBe("abc");
    expect(atob("YWI=")).toBe("ab");
    expect(atob("YQ==")).toBe("a");
  });

  test("decodes without padding (forgiving base64)", () => {
    // Missing both == padding
    expect(atob("YQ")).toBe("a");
    // Missing single = padding
    expect(atob("YWI")).toBe("ab");
    // No padding needed (length divisible by 4 content)
    expect(atob("YWJj")).toBe("abc");
  });

  test("strips ASCII whitespace before decoding", () => {
    // Spaces
    expect(atob("SGVs bG8=")).toBe("Hello");
    // Tabs
    expect(atob("SGVs\tbG8=")).toBe("Hello");
    // Newlines
    expect(atob("SGVs\nbG8=")).toBe("Hello");
    // Carriage returns
    expect(atob("SGVs\rbG8=")).toBe("Hello");
    // Form feeds
    expect(atob("SGVs\x0CbG8=")).toBe("Hello");
    // Mixed whitespace
    expect(atob(" S G V s b G 8 = ")).toBe("Hello");
  });

  test("decodes to Latin-1 characters (bytes > 0x7F)", () => {
    // Base64 for byte 0xC0 (À)
    expect(atob("wA==")).toBe(String.fromCharCode(0xC0));
    // Base64 for byte 0xFF (ÿ)
    expect(atob("/w==")).toBe(String.fromCharCode(0xFF));
    // Base64 for byte 0x80
    expect(atob("gA==")).toBe(String.fromCharCode(0x80));
  });

  test("throws InvalidCharacterError for invalid base64 characters", () => {
    expect(() => atob("!!!!")).toThrow();
    expect(() => atob("====")).toThrow();
    expect(() => atob("abc!")).toThrow();
  });

  test("throws InvalidCharacterError when length mod 4 = 1 after stripping", () => {
    // Single valid base64 char after cleanup → mod 4 = 1
    expect(() => atob("A")).toThrow();
    expect(() => atob("AAAAA")).toThrow();
  });

  test("throws TypeError when called with no arguments", () => {
    expect(() => atob()).toThrow();
  });

  test("handles only-whitespace input", () => {
    expect(atob("   ")).toBe("");
    expect(atob("\t\n")).toBe("");
  });

  test("decodes standard padding variants", () => {
    // RFC 4648 test vectors
    expect(atob("Zg==")).toBe("f");
    expect(atob("Zm8=")).toBe("fo");
    expect(atob("Zm9v")).toBe("foo");
    expect(atob("Zm9vYg==")).toBe("foob");
    expect(atob("Zm9vYmE=")).toBe("fooba");
    expect(atob("Zm9vYmFy")).toBe("foobar");
  });

  test("round-trips with btoa", () => {
    const inputs = ["", "Hello", "Hello, World!", "abc", "a", "ab", "foobar"];
    inputs.forEach((input) => {
      expect(atob(btoa(input))).toBe(input);
    });
  });

  test("round-trips binary data", () => {
    const bytes = [];
    let i = 0;
    const step = 1;
    // Test a range of byte values
    [0, 1, 127, 128, 200, 255].forEach((b) => {
      const ch = String.fromCharCode(b);
      expect(atob(btoa(ch))).toBe(ch);
    });
  });
});
