/*---
description: String.prototype.lastIndexOf works correctly
features: [String.prototype.lastIndexOf]
---*/

describe("String.prototype.lastIndexOf", () => {
  test("finds last occurrence", () => {
    expect("hello world hello".lastIndexOf("hello")).toBe(12);
    expect("hello world hello".lastIndexOf("world")).toBe(6);
  });

  test("returns -1 when not found", () => {
    expect("hello".lastIndexOf("xyz")).toBe(-1);
  });

  test("searches backwards from position", () => {
    expect("hello world hello".lastIndexOf("hello", 1)).toBe(0);
    expect("hello world hello".lastIndexOf("hello", 12)).toBe(12);
  });

  test("empty search string returns position or length", () => {
    expect("hello".lastIndexOf("")).toBe(5);
    expect("hello".lastIndexOf("", 3)).toBe(3);
  });

  test("single character search", () => {
    expect("abcabc".lastIndexOf("c")).toBe(5);
    expect("abcabc".lastIndexOf("a")).toBe(3);
  });

  test("empty string search in empty string", () => {
    expect("".lastIndexOf("")).toBe(0);
  });
});
