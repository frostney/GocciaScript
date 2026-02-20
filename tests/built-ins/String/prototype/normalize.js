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

  test("throws RangeError for invalid form", () => {
    expect(() => "abc".normalize("INVALID")).toThrow(RangeError);
  });

  test("undefined argument defaults to NFC", () => {
    expect("hello".normalize(undefined)).toBe("hello");
  });
});
