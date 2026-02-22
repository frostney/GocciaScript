/*---
description: String.prototype.isWellFormed
features: [string-is-well-formed]
---*/

describe("String.prototype.isWellFormed", () => {
  test("ASCII strings are well-formed", () => {
    expect("hello".isWellFormed()).toBe(true);
  });

  test("empty string is well-formed", () => {
    expect("".isWellFormed()).toBe(true);
  });

  test("regular strings are well-formed", () => {
    expect("abc123!@#".isWellFormed()).toBe(true);
  });

  test("string with valid surrogate pair is well-formed", () => {
    expect("\uD83D\uDE00".isWellFormed()).toBe(true);
  });

  test("lone high surrogate is not well-formed", () => {
    expect("\uD800".isWellFormed()).toBe(false);
  });

  test("lone low surrogate is not well-formed", () => {
    expect("\uDC00".isWellFormed()).toBe(false);
  });

  test("lone surrogate in the middle is not well-formed", () => {
    expect("abc\uD800def".isWellFormed()).toBe(false);
  });

  test("lone surrogate at end is not well-formed", () => {
    expect("hello\uDFFF".isWellFormed()).toBe(false);
  });

  test("multiple lone surrogates", () => {
    expect("\uD800\uD800".isWellFormed()).toBe(false);
  });

  test("high surrogate without low surrogate following", () => {
    expect("\uD800a".isWellFormed()).toBe(false);
  });

  test("low surrogate without preceding high surrogate", () => {
    expect("a\uDC00".isWellFormed()).toBe(false);
  });

  test("unicode characters are well-formed", () => {
    expect("\u00E9".isWellFormed()).toBe(true);
    expect("\u4E16\u754C".isWellFormed()).toBe(true);
  });

  test("mixed content with valid pairs is well-formed", () => {
    expect("Hello \uD83D\uDE00 World".isWellFormed()).toBe(true);
  });

  test("called on number coerces to string", () => {
    expect(String.prototype.isWellFormed.call(123)).toBe(true);
  });

  test("called on boolean coerces to string", () => {
    expect(String.prototype.isWellFormed.call(true)).toBe(true);
  });

  test("throws on undefined this", () => {
    expect(() => {
      String.prototype.isWellFormed.call(undefined);
    }).toThrow(TypeError);
  });

  test("throws on null this", () => {
    expect(() => {
      String.prototype.isWellFormed.call(null);
    }).toThrow(TypeError);
  });

  test("returns boolean", () => {
    expect(typeof "test".isWellFormed()).toBe("boolean");
  });
});
