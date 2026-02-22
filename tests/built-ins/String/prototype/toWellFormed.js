/*---
description: String.prototype.toWellFormed
features: [string-to-well-formed]
---*/

describe("String.prototype.toWellFormed", () => {
  test("well-formed string is unchanged", () => {
    expect("hello".toWellFormed()).toBe("hello");
  });

  test("empty string is unchanged", () => {
    expect("".toWellFormed()).toBe("");
  });

  test("ASCII string is unchanged", () => {
    expect("abc123".toWellFormed()).toBe("abc123");
  });

  test("valid surrogate pair is preserved", () => {
    const emoji = "\uD83D\uDE00";
    expect(emoji.toWellFormed()).toBe(emoji);
  });

  test("lone high surrogate is replaced with U+FFFD", () => {
    expect("\uD800".toWellFormed()).toBe("\uFFFD");
  });

  test("lone low surrogate is replaced with U+FFFD", () => {
    expect("\uDC00".toWellFormed()).toBe("\uFFFD");
  });

  test("lone surrogate in the middle is replaced", () => {
    const result = "abc\uD800def".toWellFormed();
    expect(result.includes("\uFFFD")).toBe(true);
    expect(result.isWellFormed()).toBe(true);
  });

  test("result is always well-formed", () => {
    expect("\uD800".toWellFormed().isWellFormed()).toBe(true);
    expect("\uDC00".toWellFormed().isWellFormed()).toBe(true);
    expect("\uD800\uD800".toWellFormed().isWellFormed()).toBe(true);
  });

  test("multiple lone surrogates are each replaced", () => {
    const result = "\uD800\uD800".toWellFormed();
    expect(result).toBe("\uFFFD\uFFFD");
  });

  test("unicode characters are preserved", () => {
    expect("\u00E9".toWellFormed()).toBe("\u00E9");
    expect("\u4E16\u754C".toWellFormed()).toBe("\u4E16\u754C");
  });

  test("mixed content with valid pairs is preserved", () => {
    const str = "Hello \uD83D\uDE00 World";
    expect(str.toWellFormed()).toBe(str);
  });

  test("called on number coerces to string", () => {
    expect(String.prototype.toWellFormed.call(123)).toBe("123");
  });

  test("called on boolean coerces to string", () => {
    expect(String.prototype.toWellFormed.call(true)).toBe("true");
  });

  test("throws on undefined this", () => {
    expect(() => {
      String.prototype.toWellFormed.call(undefined);
    }).toThrow(TypeError);
  });

  test("throws on null this", () => {
    expect(() => {
      String.prototype.toWellFormed.call(null);
    }).toThrow(TypeError);
  });

  test("returns a string", () => {
    expect(typeof "test".toWellFormed()).toBe("string");
  });
});
