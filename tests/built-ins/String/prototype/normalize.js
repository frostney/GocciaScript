describe("String.prototype.normalize", () => {
  test("returns string for NFC form", () => {
    expect("hello".normalize("NFC")).toBe("hello");
  });

  test("defaults to NFC when no argument", () => {
    expect("hello".normalize()).toBe("hello");
  });

  test("accepts all valid forms", () => {
    expect("abc".normalize("NFC")).toBe("abc");
    expect("abc".normalize("NFD")).toBe("abc");
    expect("abc".normalize("NFKC")).toBe("abc");
    expect("abc".normalize("NFKD")).toBe("abc");
  });

  test("normalizes canonical forms", () => {
    expect("e\u0301".normalize("NFC")).toBe("\u00E9");
    expect("\u00E9".normalize("NFD")).toBe("e\u0301");
  });

  test("normalizes compatibility forms", () => {
    expect("\uFB01".normalize("NFKD")).toBe("fi");
    expect("\u212B".normalize("NFKC")).toBe("\u00C5");
  });

  test("recursively decomposes and canonically orders combining marks", () => {
    expect("\u01FA".normalize("NFD")).toBe("A\u030A\u0301");
    expect("q\u0307\u0323".normalize("NFD")).toBe("q\u0323\u0307");
  });

  test("normalizes Hangul algorithmically", () => {
    expect("\uAC01".normalize("NFD")).toBe("\u1100\u1161\u11A8");
    expect("\u1100\u1161\u11A8".normalize("NFC")).toBe("\uAC01");
  });

  test("preserves unpaired UTF-16 surrogates", () => {
    expect("\uD800a\uDC00".normalize("NFKC")).toBe("\uD800a\uDC00");
  });

  test("throws RangeError for invalid form", () => {
    expect(() => "abc".normalize("INVALID")).toThrow(RangeError);
  });

  test("undefined argument defaults to NFC", () => {
    expect("hello".normalize(undefined)).toBe("hello");
  });
});
