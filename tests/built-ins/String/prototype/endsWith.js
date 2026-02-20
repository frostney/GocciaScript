/*---
description: String.prototype.endsWith works correctly
features: [String.prototype.endsWith]
---*/

describe("String.prototype.endsWith", () => {
  test("basic matching", () => {
    expect("hello world".endsWith("world")).toBe(true);
    expect("hello world".endsWith("hello")).toBe(false);
  });

  test("with endPosition argument", () => {
    expect("hello world".endsWith("hello", 5)).toBe(true);
    expect("hello world".endsWith("world", 5)).toBe(false);
  });

  test("empty search string always true", () => {
    expect("hello".endsWith("")).toBe(true);
    expect("".endsWith("")).toBe(true);
  });

  test("search string longer than string", () => {
    expect("a".endsWith("ab")).toBe(false);
  });

  test("case sensitive", () => {
    expect("Hello".endsWith("hello")).toBe(false);
  });

  test("entire string match", () => {
    expect("hello".endsWith("hello")).toBe(true);
  });
});
