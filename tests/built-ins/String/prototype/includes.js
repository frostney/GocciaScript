/*---
description: String.prototype.includes works correctly
features: [String.prototype.includes]
---*/

describe("String.prototype.includes", () => {
  test("finds substring", () => {
    expect("hello world".includes("hello")).toBe(true);
    expect("hello world".includes("world")).toBe(true);
    expect("hello world".includes("xyz")).toBe(false);
  });

  test("with position argument", () => {
    expect("hello world".includes("hello", 1)).toBe(false);
    expect("hello world".includes("world", 6)).toBe(true);
    expect("hello world".includes("o", 4)).toBe(true);
  });

  test("position beyond string length", () => {
    expect("hello world".includes("hello", 12)).toBe(false);
  });

  test("empty search string always found", () => {
    expect("hello".includes("")).toBe(true);
    expect("hello".includes("", 100)).toBe(true);
    expect("".includes("")).toBe(true);
  });

  test("case sensitive", () => {
    expect("Hello".includes("hello")).toBe(false);
  });

  test("empty string does not include non-empty", () => {
    expect("".includes("a")).toBe(false);
  });
});
