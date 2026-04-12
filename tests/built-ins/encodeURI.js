describe("encodeURI", () => {
  test("typeof encodeURI is function", () => {
    expect(typeof encodeURI).toBe("function");
  });

  test("returns empty string for empty input", () => {
    expect(encodeURI("")).toBe("");
  });

  test("does not encode unreserved characters (A-Z a-z 0-9)", () => {
    expect(encodeURI("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")).toBe(
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
    );
  });

  test("does not encode URI mark characters (- _ . ! ~ * ' ( ))", () => {
    expect(encodeURI("-_.!~*'()")).toBe("-_.!~*'()");
  });

  test("does not encode reserved URI characters (;/?:@&=+$,#)", () => {
    expect(encodeURI(";")).toBe(";");
    expect(encodeURI("/")).toBe("/");
    expect(encodeURI("?")).toBe("?");
    expect(encodeURI(":")).toBe(":");
    expect(encodeURI("@")).toBe("@");
    expect(encodeURI("&")).toBe("&");
    expect(encodeURI("=")).toBe("=");
    expect(encodeURI("+")).toBe("+");
    expect(encodeURI("$")).toBe("$");
    expect(encodeURI(",")).toBe(",");
    expect(encodeURI("#")).toBe("#");
  });

  test("encodes spaces and non-reserved characters", () => {
    expect(encodeURI(" ")).toBe("%20");
    expect(encodeURI("<")).toBe("%3C");
    expect(encodeURI(">")).toBe("%3E");
    expect(encodeURI('"')).toBe("%22");
    expect(encodeURI("{")).toBe("%7B");
    expect(encodeURI("}")).toBe("%7D");
    expect(encodeURI("|")).toBe("%7C");
    expect(encodeURI("\\")).toBe("%5C");
    expect(encodeURI("^")).toBe("%5E");
    expect(encodeURI("`")).toBe("%60");
  });

  test("preserves complete URIs", () => {
    expect(encodeURI("https://example.com/path?query=value&other=123#fragment")).toBe(
      "https://example.com/path?query=value&other=123#fragment"
    );
    expect(encodeURI("http://user:pass@host.com:8080/p/a/t/h?q=1")).toBe(
      "http://user:pass@host.com:8080/p/a/t/h?q=1"
    );
  });

  test("encodes spaces in URIs", () => {
    expect(encodeURI("https://example.com/hello world")).toBe(
      "https://example.com/hello%20world"
    );
  });

  test("encodes multi-byte UTF-8 characters in URIs", () => {
    // 2-byte UTF-8: U+00E9 (é) → %C3%A9
    expect(encodeURI("\u00E9")).toBe("%C3%A9");
    // 3-byte UTF-8: U+4E2D (中) → %E4%B8%AD
    expect(encodeURI("\u4E2D")).toBe("%E4%B8%AD");
    // Real-world example: Japanese characters in a URL
    expect(encodeURI("https://example.com/\u4E2D\u6587")).toBe(
      "https://example.com/%E4%B8%AD%E6%96%87"
    );
  });

  test("encodes percent sign itself", () => {
    expect(encodeURI("%")).toBe("%25");
  });

  test("coerces non-string arguments to string", () => {
    expect(encodeURI(123)).toBe("123");
    expect(encodeURI(true)).toBe("true");
    expect(encodeURI(null)).toBe("null");
  });

  test("encodes undefined when called with no arguments", () => {
    expect(encodeURI()).toBe("undefined");
  });

  test("round-trips with decodeURI", () => {
    const inputs = [
      "https://example.com/path?query=value&other=123#fragment",
      "https://example.com/hello world",
      "https://example.com/\u00E9",
      "https://example.com/\u4E2D\u6587",
    ];
    inputs.forEach((input) => {
      expect(decodeURI(encodeURI(input))).toBe(input);
    });
  });

  test("difference from encodeURIComponent: preserves reserved characters", () => {
    const uri = "https://example.com/path?q=hello world#frag";
    const encoded = encodeURI(uri);
    // encodeURI preserves :, /, ?, #, etc.
    expect(encoded).toContain("://");
    expect(encoded).toContain("?");
    expect(encoded).toContain("#");
    expect(encoded).toContain("%20");
    // encodeURIComponent would encode all of those
    const component = encodeURIComponent(uri);
    expect(component).toContain("%3A");
    expect(component).toContain("%2F");
    expect(component).toContain("%3F");
    expect(component).toContain("%23");
  });
});
