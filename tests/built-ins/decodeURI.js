describe("decodeURI", () => {
  test("typeof decodeURI is function", () => {
    expect(typeof decodeURI).toBe("function");
  });

  test("returns empty string for empty input", () => {
    expect(decodeURI("")).toBe("");
  });

  test("passes through unencoded characters unchanged", () => {
    expect(decodeURI("hello")).toBe("hello");
    expect(decodeURI("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")).toBe(
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
    );
  });

  test("decodes non-reserved percent-encoded characters", () => {
    expect(decodeURI("%20")).toBe(" ");
    expect(decodeURI("%3C")).toBe("<");
    expect(decodeURI("%3E")).toBe(">");
    expect(decodeURI("%22")).toBe('"');
    expect(decodeURI("%7B")).toBe("{");
    expect(decodeURI("%7D")).toBe("}");
    expect(decodeURI("%7C")).toBe("|");
    expect(decodeURI("%5C")).toBe("\\");
    expect(decodeURI("%5E")).toBe("^");
    expect(decodeURI("%60")).toBe("`");
    expect(decodeURI("%25")).toBe("%");
  });

  test("does NOT decode reserved URI characters (;/?:@&=+$,#)", () => {
    expect(decodeURI("%3B")).toBe("%3B"); // ;
    expect(decodeURI("%2F")).toBe("%2F"); // /
    expect(decodeURI("%3F")).toBe("%3F"); // ?
    expect(decodeURI("%3A")).toBe("%3A"); // :
    expect(decodeURI("%40")).toBe("%40"); // @
    expect(decodeURI("%26")).toBe("%26"); // &
    expect(decodeURI("%3D")).toBe("%3D"); // =
    expect(decodeURI("%2B")).toBe("%2B"); // +
    expect(decodeURI("%24")).toBe("%24"); // $
    expect(decodeURI("%2C")).toBe("%2C"); // ,
    expect(decodeURI("%23")).toBe("%23"); // #
  });

  test("preserves original hex case for reserved characters", () => {
    // ES2026 §19.2.6.2: reserved escapes are preserved verbatim, not normalized
    expect(decodeURI("%3b")).toBe("%3b"); // ;
    expect(decodeURI("%2f")).toBe("%2f"); // /
    expect(decodeURI("%3f")).toBe("%3f"); // ?
    expect(decodeURI("%3B")).toBe("%3B"); // ; (uppercase stays uppercase)
    expect(decodeURI("%2F")).toBe("%2F"); // / (uppercase stays uppercase)
  });

  test("decodes multi-byte UTF-8 sequences", () => {
    // 2-byte: U+00E9 (é) → %C3%A9
    expect(decodeURI("%C3%A9")).toBe("\u00E9");
    // 3-byte: U+4E2D (中) → %E4%B8%AD
    expect(decodeURI("%E4%B8%AD")).toBe("\u4E2D");
  });

  test("preserves complete URIs while decoding unreserved characters", () => {
    expect(decodeURI("https://example.com/hello%20world?q=1&b=2#frag")).toBe(
      "https://example.com/hello world?q=1&b=2#frag"
    );
  });

  test("throws URIError for incomplete percent sequence", () => {
    expect(() => decodeURI("%")).toThrow(URIError);
    expect(() => decodeURI("%2")).toThrow(URIError);
    expect(() => decodeURI("abc%")).toThrow(URIError);
  });

  test("throws URIError for invalid hex digits", () => {
    expect(() => decodeURI("%GG")).toThrow(URIError);
    expect(() => decodeURI("%ZZ")).toThrow(URIError);
  });

  test("throws URIError for invalid UTF-8 sequences", () => {
    // Invalid continuation byte
    expect(() => decodeURI("%C3%00")).toThrow(URIError);
    // Truncated multi-byte sequence
    expect(() => decodeURI("%E4%B8")).toThrow(URIError);
    // Invalid lead byte (bare continuation byte)
    expect(() => decodeURI("%80")).toThrow(URIError);
  });

  test("throws URIError for overlong UTF-8 encodings", () => {
    // Overlong 2-byte encoding of U+0000
    expect(() => decodeURI("%C0%80")).toThrow(URIError);
  });

  test("coerces non-string arguments to string", () => {
    expect(decodeURI(123)).toBe("123");
    expect(decodeURI(true)).toBe("true");
    expect(decodeURI(null)).toBe("null");
  });

  test("decodes undefined when called with no arguments", () => {
    expect(decodeURI()).toBe("undefined");
  });

  test("round-trips with encodeURI", () => {
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

  test("difference from decodeURIComponent: preserves reserved character encoding", () => {
    // decodeURI does NOT decode %2F (/)
    expect(decodeURI("%2F")).toBe("%2F");
    // decodeURIComponent DOES decode %2F (/)
    expect(decodeURIComponent("%2F")).toBe("/");
  });
});
