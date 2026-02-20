/*---
description: String.prototype.startsWith works correctly
features: [String.prototype.startsWith]
---*/

describe("String.prototype.startsWith", () => {
  test("basic matching", () => {
    expect("hello world".startsWith("hello")).toBe(true);
    expect("hello world".startsWith("world")).toBe(false);
  });

  test("with position argument", () => {
    expect("hello world".startsWith("world", 6)).toBe(true);
    expect("hello world".startsWith("hello", 1)).toBe(false);
  });

  test("empty search string always true", () => {
    expect("hello".startsWith("")).toBe(true);
    expect("".startsWith("")).toBe(true);
  });

  test("search string longer than string", () => {
    expect("a".startsWith("ab")).toBe(false);
  });

  test("case sensitive", () => {
    expect("Hello".startsWith("hello")).toBe(false);
  });
});
