/*---
description: String.prototype.indexOf works correctly
features: [String.prototype.indexOf]
---*/

describe("String.prototype.indexOf", () => {
  test("finds substring from beginning", () => {
    expect("hello world".indexOf("hello")).toBe(0);
    expect("hello world".indexOf("world")).toBe(6);
  });

  test("returns -1 when not found", () => {
    expect("hello world".indexOf("xyz")).toBe(-1);
  });

  test("searches from position", () => {
    expect("hello world".indexOf("hello", 1)).toBe(-1);
    expect("hello hello".indexOf("hello", 1)).toBe(6);
  });

  test("empty search string returns position", () => {
    expect("hello".indexOf("")).toBe(0);
    expect("hello".indexOf("", 3)).toBe(3);
  });

  test("empty search string beyond length returns length", () => {
    expect("hello".indexOf("", 100)).toBe(5);
  });

  test("negative position treated as 0", () => {
    expect("hello".indexOf("h", -5)).toBe(0);
  });

  test("position beyond length returns -1", () => {
    expect("hello".indexOf("h", 100)).toBe(-1);
  });

  test("finds single character", () => {
    expect("abcde".indexOf("c")).toBe(2);
    expect("aaa".indexOf("a")).toBe(0);
  });

  test("empty string indexOf empty string", () => {
    expect("".indexOf("")).toBe(0);
  });

  test("empty string indexOf non-empty returns -1", () => {
    expect("".indexOf("a")).toBe(-1);
  });
});
