/*---
description: RegExp.escape
features: [RegExp]
---*/

describe("RegExp.escape", () => {
  test("is a function with length 1", () => {
    expect(typeof RegExp.escape).toBe("function");
    expect(RegExp.escape.length).toBe(1);
  });

  test("escapes syntax characters with backslash", () => {
    expect(RegExp.escape("^")).toBe("\\^");
    expect(RegExp.escape("$")).toBe("\\$");
    expect(RegExp.escape("\\")).toBe("\\\\");
    expect(RegExp.escape(".")).toBe("\\.");
    expect(RegExp.escape("*")).toBe("\\*");
    expect(RegExp.escape("+")).toBe("\\+");
    expect(RegExp.escape("?")).toBe("\\?");
    expect(RegExp.escape("(")).toBe("\\(");
    expect(RegExp.escape(")")).toBe("\\)");
    expect(RegExp.escape("[")).toBe("\\[");
    expect(RegExp.escape("]")).toBe("\\]");
    expect(RegExp.escape("{")).toBe("\\{");
    expect(RegExp.escape("}")).toBe("\\}");
    expect(RegExp.escape("|")).toBe("\\|");
    expect(RegExp.escape("/")).toBe("\\/");
  });

  test("hex-encodes the first character when it is a digit", () => {
    expect(RegExp.escape("1")).toBe("\\x31");
    expect(RegExp.escape("0")).toBe("\\x30");
    expect(RegExp.escape("9")).toBe("\\x39");
    expect(RegExp.escape("5abc")).toBe("\\x35abc");
  });

  test("hex-encodes the first character when it is an ASCII letter", () => {
    expect(RegExp.escape("a")).toBe("\\x61");
    expect(RegExp.escape("z")).toBe("\\x7a");
    expect(RegExp.escape("A")).toBe("\\x41");
    expect(RegExp.escape("Z")).toBe("\\x5a");
    expect(RegExp.escape("hello")).toBe("\\x68ello");
  });

  test("does not hex-encode digits/letters after the first character", () => {
    expect(RegExp.escape(".1")).toBe("\\.1");
    expect(RegExp.escape(".a")).toBe("\\.a");
    expect(RegExp.escape("^abc123")).toBe("\\^abc123");
  });

  test("hex-encodes ClassSetReservedPunctuator characters", () => {
    expect(RegExp.escape("&")).toBe("\\x26");
    expect(RegExp.escape("-")).toBe("\\x2d");
    expect(RegExp.escape("!")).toBe("\\x21");
    expect(RegExp.escape("#")).toBe("\\x23");
    expect(RegExp.escape("%")).toBe("\\x25");
    expect(RegExp.escape(",")).toBe("\\x2c");
    expect(RegExp.escape(":")).toBe("\\x3a");
    expect(RegExp.escape(";")).toBe("\\x3b");
    expect(RegExp.escape("<")).toBe("\\x3c");
    expect(RegExp.escape("=")).toBe("\\x3d");
    expect(RegExp.escape(">")).toBe("\\x3e");
    expect(RegExp.escape("@")).toBe("\\x40");
    expect(RegExp.escape("`")).toBe("\\x60");
    expect(RegExp.escape("~")).toBe("\\x7e");
  });

  test("hex-encodes ASCII whitespace and line terminators", () => {
    expect(RegExp.escape("\t")).toBe("\\x09");
    expect(RegExp.escape("\n")).toBe("\\x0a");
    expect(RegExp.escape("\x0b")).toBe("\\x0b");
    expect(RegExp.escape("\x0c")).toBe("\\x0c");
    expect(RegExp.escape("\r")).toBe("\\x0d");
    expect(RegExp.escape(" ")).toBe("\\x20");
  });

  test("returns empty string for empty input", () => {
    expect(RegExp.escape("")).toBe("");
  });

  test("passes through normal characters", () => {
    expect(RegExp.escape("_")).toBe("_");
    expect(RegExp.escape("'")).toBe("'");
    expect(RegExp.escape('"')).toBe('"');
  });

  test("escapes mixed content correctly", () => {
    expect(RegExp.escape("foo.bar+baz*qux"))
      .toBe("\\x66oo\\.bar\\+baz\\*qux");
    expect(RegExp.escape("(test)"))
      .toBe("\\(test\\)");
    expect(RegExp.escape("[a-z]"))
      .toBe("\\[a\\x2dz\\]");
  });

  test("produces a usable regex pattern", () => {
    const special = "Hello. How are you? (I'm fine!)";
    const escaped = RegExp.escape(special);
    const regex = new RegExp(escaped);
    expect(regex.test(special)).toBe(true);
    expect(regex.test("Hello! How are you? (I'm fine!)")).toBe(false);
  });

  test("escaped string works inside character class context", () => {
    const input = "a+b";
    const escaped = RegExp.escape(input);
    const regex = new RegExp("^" + escaped + "$");
    expect(regex.test("a+b")).toBe(true);
    expect(regex.test("ab")).toBe(false);
    expect(regex.test("aab")).toBe(false);
  });

  test("throws TypeError for non-string argument", () => {
    expect(() => { RegExp.escape(42); }).toThrow(TypeError);
    expect(() => { RegExp.escape(true); }).toThrow(TypeError);
    expect(() => { RegExp.escape(null); }).toThrow(TypeError);
    expect(() => { RegExp.escape(undefined); }).toThrow(TypeError);
    expect(() => { RegExp.escape({}); }).toThrow(TypeError);
    expect(() => { RegExp.escape(); }).toThrow(TypeError);
  });

  test("first character encoding does not apply to syntax characters", () => {
    // Syntax characters as first char use backslash, not hex
    expect(RegExp.escape("^abc")).toBe("\\^abc");
    expect(RegExp.escape(".test")).toBe("\\.test");
    expect(RegExp.escape("*foo")).toBe("\\*foo");
  });

  test("first character encoding does not apply to ClassSetReservedPunctuators", () => {
    // ClassSetReservedPunctuators as first char use hex encoding
    expect(RegExp.escape("&abc")).toBe("\\x26abc");
    expect(RegExp.escape("-test")).toBe("\\x2dtest");
  });
});
