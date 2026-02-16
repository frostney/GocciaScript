/*---
description: Number.parseInt function works correctly
features: [Number.parseInt]
---*/

describe("Number.parseInt", () => {
  test("parses integer strings", () => {
    expect(Number.parseInt("123")).toBe(123);
    expect(Number.parseInt("42")).toBe(42);
    expect(Number.parseInt("-7")).toBe(-7);
  });

  test("truncates float strings", () => {
    expect(Number.parseInt("123.45")).toBe(123);
  });

  test("parses with radix", () => {
    expect(Number.parseInt("FF", 16)).toBe(255);
    expect(Number.parseInt("0x1A")).toBe(26);
    expect(Number.parseInt("10", 2)).toBe(2);
    expect(Number.parseInt("10", 8)).toBe(8);
    expect(Number.parseInt("10", 10)).toBe(10);
    expect(Number.parseInt("123", 8)).toBe(83);
    expect(Number.parseInt("123", 16)).toBe(291);
    expect(Number.parseInt("123", 2)).toBe(1);
  });

  test("stops at non-numeric characters", () => {
    expect(Number.parseInt("123abc")).toBe(123);
    expect(Number.parseInt("10px")).toBe(10);
  });

  test("returns NaN for non-parseable strings", () => {
    expect(Number.isNaN(Number.parseInt("abc"))).toBe(true);
    expect(Number.isNaN(Number.parseInt(""))).toBe(true);
  });

  test("handles leading whitespace", () => {
    expect(Number.parseInt("  42")).toBe(42);
  });
});
