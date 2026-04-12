describe("decodeURIComponent", () => {
  test("typeof decodeURIComponent is function", () => {
    expect(typeof decodeURIComponent).toBe("function");
  });

  test("returns empty string for empty input", () => {
    expect(decodeURIComponent("")).toBe("");
  });

  test("passes through unencoded characters unchanged", () => {
    expect(decodeURIComponent("hello")).toBe("hello");
    expect(decodeURIComponent("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")).toBe(
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
    );
  });

  test("decodes percent-encoded ASCII characters", () => {
    expect(decodeURIComponent("%20")).toBe(" ");
    expect(decodeURIComponent("%3B")).toBe(";");
    expect(decodeURIComponent("%2F")).toBe("/");
    expect(decodeURIComponent("%3F")).toBe("?");
    expect(decodeURIComponent("%3A")).toBe(":");
    expect(decodeURIComponent("%40")).toBe("@");
    expect(decodeURIComponent("%26")).toBe("&");
    expect(decodeURIComponent("%3D")).toBe("=");
    expect(decodeURIComponent("%2B")).toBe("+");
    expect(decodeURIComponent("%24")).toBe("$");
    expect(decodeURIComponent("%2C")).toBe(",");
    expect(decodeURIComponent("%23")).toBe("#");
    expect(decodeURIComponent("%25")).toBe("%");
  });

  test("decodes lowercase hex digits", () => {
    expect(decodeURIComponent("%2f")).toBe("/");
    expect(decodeURIComponent("%3a")).toBe(":");
    expect(decodeURIComponent("%2b")).toBe("+");
  });

  test("decodes multi-byte UTF-8 sequences", () => {
    // 2-byte: U+00E9 (é) → %C3%A9
    expect(decodeURIComponent("%C3%A9")).toBe("\u00E9");
    // 3-byte: U+4E2D (中) → %E4%B8%AD
    expect(decodeURIComponent("%E4%B8%AD")).toBe("\u4E2D");
  });

  test("decodes mixed encoded and plain text", () => {
    expect(decodeURIComponent("hello%20world")).toBe("hello world");
    expect(decodeURIComponent("key%3Dvalue%26foo%3Dbar")).toBe("key=value&foo=bar");
    expect(decodeURIComponent("path%2Fto%2Ffile")).toBe("path/to/file");
  });

  test("throws URIError for incomplete percent sequence", () => {
    expect(() => decodeURIComponent("%")).toThrow(URIError);
    expect(() => decodeURIComponent("%2")).toThrow(URIError);
    expect(() => decodeURIComponent("abc%")).toThrow(URIError);
    expect(() => decodeURIComponent("abc%2")).toThrow(URIError);
  });

  test("throws URIError for invalid hex digits", () => {
    expect(() => decodeURIComponent("%GG")).toThrow(URIError);
    expect(() => decodeURIComponent("%ZZ")).toThrow(URIError);
    expect(() => decodeURIComponent("%0G")).toThrow(URIError);
  });

  test("throws URIError for invalid UTF-8 sequences", () => {
    // Invalid continuation byte
    expect(() => decodeURIComponent("%C3%00")).toThrow(URIError);
    // Truncated multi-byte sequence: 3-byte lead with only 1 continuation
    expect(() => decodeURIComponent("%E4%B8")).toThrow(URIError);
    // Invalid lead byte (bare continuation byte)
    expect(() => decodeURIComponent("%80")).toThrow(URIError);
  });

  test("throws URIError for overlong UTF-8 encodings", () => {
    // Overlong 2-byte encoding of U+0000 (should be 1 byte)
    expect(() => decodeURIComponent("%C0%80")).toThrow(URIError);
  });

  test("coerces non-string arguments to string", () => {
    expect(decodeURIComponent(123)).toBe("123");
    expect(decodeURIComponent(true)).toBe("true");
    expect(decodeURIComponent(null)).toBe("null");
  });

  test("decodes undefined when called with no arguments", () => {
    expect(decodeURIComponent()).toBe("undefined");
  });

  test("round-trips with encodeURIComponent", () => {
    const inputs = [
      "hello world",
      "key=value&foo=bar",
      "path/to/file?q=search#hash",
      "\u00E9",
      "\u4E2D\u6587",
      "-_.!~*'()",
    ];
    inputs.forEach((input) => {
      expect(decodeURIComponent(encodeURIComponent(input))).toBe(input);
    });
  });
});
