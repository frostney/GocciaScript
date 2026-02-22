/*---
description: Math.sumPrecise returns a precise sum of iterable elements
features: [Math.sumPrecise]
---*/

const isGocciaScript = typeof GocciaScript !== "undefined";

describe.runIf(isGocciaScript)("Math.sumPrecise", () => {
  test("basic summation", () => {
    expect(Math.sumPrecise([1, 2, 3])).toBe(6);
    expect(Math.sumPrecise([10, 20, 30])).toBe(60);
    expect(Math.sumPrecise([0.1, 0.2])).toBe(0.30000000000000004);
  });

  test("precise summation avoids floating point error", () => {
    const result = Math.sumPrecise([1e20, 0.1, -1e20]);
    expect(result).toBe(0.1);
  });

  test("single element", () => {
    expect(Math.sumPrecise([42])).toBe(42);
    expect(Math.sumPrecise([-5])).toBe(-5);
  });

  test("empty iterable returns negative zero", () => {
    const result = Math.sumPrecise([]);
    expect(Object.is(result, -0)).toBe(true);
  });

  test("NaN propagation", () => {
    expect(Number.isNaN(Math.sumPrecise([1, NaN, 3]))).toBe(true);
    expect(Number.isNaN(Math.sumPrecise([NaN]))).toBe(true);
  });

  test("positive Infinity", () => {
    expect(Math.sumPrecise([1, Infinity, 3])).toBe(Infinity);
    expect(Math.sumPrecise([Infinity])).toBe(Infinity);
    expect(Math.sumPrecise([Infinity, Infinity])).toBe(Infinity);
  });

  test("negative Infinity", () => {
    expect(Math.sumPrecise([1, -Infinity, 3])).toBe(-Infinity);
    expect(Math.sumPrecise([-Infinity])).toBe(-Infinity);
  });

  test("mixed Infinity returns NaN", () => {
    expect(Number.isNaN(Math.sumPrecise([Infinity, -Infinity]))).toBe(true);
    expect(Number.isNaN(Math.sumPrecise([1, Infinity, -Infinity, 2]))).toBe(true);
  });

  test("works with Set iterable", () => {
    const s = new Set([1, 2, 3]);
    expect(Math.sumPrecise(s)).toBe(6);
  });

  test("throws TypeError for non-number elements", () => {
    expect(() => Math.sumPrecise([1, "2", 3])).toThrow();
    expect(() => Math.sumPrecise([true])).toThrow();
    expect(() => Math.sumPrecise([null])).toThrow();
    expect(() => Math.sumPrecise([undefined])).toThrow();
    expect(() => Math.sumPrecise([{}])).toThrow();
  });

  test("throws TypeError for non-iterable argument", () => {
    expect(() => Math.sumPrecise(42)).toThrow();
    expect(() => Math.sumPrecise(null)).toThrow();
  });

  test("sum of many values", () => {
    expect(Math.sumPrecise([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])).toBe(55);
    expect(Math.sumPrecise([100, 200, 300, 400])).toBe(1000);
  });
});
