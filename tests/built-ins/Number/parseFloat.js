/*---
description: Number.parseFloat function works correctly
features: [Number.parseFloat]
---*/

describe("Number.parseFloat", () => {
  test("parses float strings", () => {
    expect(Number.parseFloat("3.14")).toBe(3.14);
    expect(Number.parseFloat("123.45")).toBe(123.45);
    expect(Number.parseFloat("0.5")).toBe(0.5);
    expect(Number.parseFloat("-2.7")).toBe(-2.7);
  });

  test("parses integer strings as floats", () => {
    expect(Number.parseFloat("123")).toBe(123);
    expect(Number.parseFloat("42")).toBe(42);
  });

  test("stops at non-numeric characters", () => {
    expect(Number.parseFloat("3.14abc")).toBe(3.14);
    expect(Number.parseFloat("123.45abc")).toBe(123.45);
  });

  test("returns NaN for non-parseable strings", () => {
    expect(Number.isNaN(Number.parseFloat("abc"))).toBe(true);
  });
});
