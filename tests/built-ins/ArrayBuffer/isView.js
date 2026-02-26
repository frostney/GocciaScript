/*---
description: ArrayBuffer.isView
features: [ArrayBuffer]
---*/

describe("ArrayBuffer.isView", () => {
  test("returns true for typed arrays", () => {
    expect(ArrayBuffer.isView(new Int8Array(1))).toBe(true);
    expect(ArrayBuffer.isView(new Uint8Array(1))).toBe(true);
    expect(ArrayBuffer.isView(new Int32Array(1))).toBe(true);
    expect(ArrayBuffer.isView(new Float64Array(1))).toBe(true);
  });

  test("returns false for ArrayBuffer", () => {
    const buf = new ArrayBuffer(8);
    expect(ArrayBuffer.isView(buf)).toBe(false);
  });

  test("returns false for plain object", () => {
    expect(ArrayBuffer.isView({})).toBe(false);
  });

  test("returns false for array", () => {
    expect(ArrayBuffer.isView([1, 2, 3])).toBe(false);
  });

  test("returns false for number", () => {
    expect(ArrayBuffer.isView(42)).toBe(false);
  });

  test("returns false for string", () => {
    expect(ArrayBuffer.isView("hello")).toBe(false);
  });

  test("returns false for null", () => {
    expect(ArrayBuffer.isView(null)).toBe(false);
  });

  test("returns false for undefined", () => {
    expect(ArrayBuffer.isView(undefined)).toBe(false);
  });

  test("with no arguments returns false", () => {
    expect(ArrayBuffer.isView()).toBe(false);
  });

  test("returns false for boolean", () => {
    expect(ArrayBuffer.isView(true)).toBe(false);
  });
});
