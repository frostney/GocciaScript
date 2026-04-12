describe("encodeURIComponent", () => {
  test("typeof encodeURIComponent is function", () => {
    expect(typeof encodeURIComponent).toBe("function");
  });

  test("returns empty string for empty input", () => {
    expect(encodeURIComponent("")).toBe("");
  });

  test("does not encode unreserved characters (A-Z a-z 0-9 - _ . ! ~ * ' ( ))", () => {
    expect(encodeURIComponent("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")).toBe(
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
    );
    expect(encodeURIComponent("-_.!~*'()")).toBe("-_.!~*'()");
  });

  test("encodes reserved URI characters", () => {
    expect(encodeURIComponent(";")).toBe("%3B");
    expect(encodeURIComponent("/")).toBe("%2F");
    expect(encodeURIComponent("?")).toBe("%3F");
    expect(encodeURIComponent(":")).toBe("%3A");
    expect(encodeURIComponent("@")).toBe("%40");
    expect(encodeURIComponent("&")).toBe("%26");
    expect(encodeURIComponent("=")).toBe("%3D");
    expect(encodeURIComponent("+")).toBe("%2B");
    expect(encodeURIComponent("$")).toBe("%24");
    expect(encodeURIComponent(",")).toBe("%2C");
    expect(encodeURIComponent("#")).toBe("%23");
  });

  test("encodes spaces", () => {
    expect(encodeURIComponent(" ")).toBe("%20");
    expect(encodeURIComponent("hello world")).toBe("hello%20world");
  });

  test("encodes special characters", () => {
    expect(encodeURIComponent("<")).toBe("%3C");
    expect(encodeURIComponent(">")).toBe("%3E");
    expect(encodeURIComponent('"')).toBe("%22");
    expect(encodeURIComponent("{")).toBe("%7B");
    expect(encodeURIComponent("}")).toBe("%7D");
    expect(encodeURIComponent("|")).toBe("%7C");
    expect(encodeURIComponent("\\")).toBe("%5C");
    expect(encodeURIComponent("^")).toBe("%5E");
    expect(encodeURIComponent("`")).toBe("%60");
  });

  test("encodes multi-byte UTF-8 characters", () => {
    // 2-byte UTF-8: U+00E9 (é) → %C3%A9
    expect(encodeURIComponent("\u00E9")).toBe("%C3%A9");
    // 3-byte UTF-8: U+4E2D (中) → %E4%B8%AD
    expect(encodeURIComponent("\u4E2D")).toBe("%E4%B8%AD");
  });

  test("encodes percent sign itself", () => {
    expect(encodeURIComponent("%")).toBe("%25");
  });

  test("encodes practical URI component values", () => {
    expect(encodeURIComponent("key=value&foo=bar")).toBe("key%3Dvalue%26foo%3Dbar");
    expect(encodeURIComponent("hello world!")).toBe("hello%20world!");
    expect(encodeURIComponent("path/to/file")).toBe("path%2Fto%2Ffile");
  });

  test("coerces non-string arguments to string", () => {
    expect(encodeURIComponent(123)).toBe("123");
    expect(encodeURIComponent(true)).toBe("true");
    expect(encodeURIComponent(null)).toBe("null");
    expect(encodeURIComponent(undefined)).toBe("undefined");
  });

  test("encodes undefined when called with no arguments", () => {
    expect(encodeURIComponent()).toBe("undefined");
  });

  test("throws URIError for lone surrogate input", () => {
    expect(() => encodeURIComponent("\uD800")).toThrow(URIError);
    expect(() => encodeURIComponent("\uDFFF")).toThrow(URIError);
    expect(() => encodeURIComponent("abc\uD800")).toThrow(URIError);
    expect(() => encodeURIComponent("\uDC00xyz")).toThrow(URIError);
  });

  test("round-trips with decodeURIComponent", () => {
    const inputs = [
      "hello world",
      "key=value&foo=bar",
      "path/to/file?q=search#hash",
      "\u00E9",
      "\u4E2D\u6587",
    ];
    inputs.forEach((input) => {
      expect(decodeURIComponent(encodeURIComponent(input))).toBe(input);
    });
  });
});
