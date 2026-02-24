/*---
description: ArrayBuffer constructor
features: [ArrayBuffer]
---*/

describe("ArrayBuffer constructor", () => {
  test("creates an ArrayBuffer with the specified byte length", () => {
    const buf = new ArrayBuffer(8);
    expect(buf.byteLength).toBe(8);
  });

  test("creates an empty ArrayBuffer with length 0", () => {
    const buf = new ArrayBuffer(0);
    expect(buf.byteLength).toBe(0);
  });

  test("creates a large ArrayBuffer", () => {
    const buf = new ArrayBuffer(1024);
    expect(buf.byteLength).toBe(1024);
  });

  test("creates a buffer with length 1", () => {
    const buf = new ArrayBuffer(1);
    expect(buf.byteLength).toBe(1);
  });

  test("typeof returns 'object'", () => {
    const buf = new ArrayBuffer(4);
    expect(typeof buf).toBe("object");
  });

  test("instanceof ArrayBuffer", () => {
    const buf = new ArrayBuffer(4);
    expect(buf instanceof ArrayBuffer).toBe(true);
  });

  test("byteLength is read-only (reflects allocated size)", () => {
    const buf = new ArrayBuffer(16);
    expect(buf.byteLength).toBe(16);
  });
});

describe("ArrayBuffer constructor error cases", () => {
  test("throws RangeError for negative length", () => {
    expect(() => new ArrayBuffer(-1)).toThrow(RangeError);
  });

  test("throws RangeError for large negative length", () => {
    expect(() => new ArrayBuffer(-100)).toThrow(RangeError);
  });

  test("throws RangeError for Infinity", () => {
    expect(() => new ArrayBuffer(Infinity)).toThrow(RangeError);
  });

  test("throws RangeError for -Infinity", () => {
    expect(() => new ArrayBuffer(-Infinity)).toThrow(RangeError);
  });
});

describe("ArrayBuffer constructor ToIndex coercion (ES2026 §6.2.4.2)", () => {
  test("no arguments: ToIndex(undefined) produces 0", () => {
    const buf = new ArrayBuffer();
    expect(buf.byteLength).toBe(0);
  });

  test("NaN: ToIntegerOrInfinity(NaN) produces 0", () => {
    const buf = new ArrayBuffer(NaN);
    expect(buf.byteLength).toBe(0);
  });

  test("non-integer float 1.5: truncated to 1", () => {
    const buf = new ArrayBuffer(1.5);
    expect(buf.byteLength).toBe(1);
  });

  test("negative float -0.5: truncated to 0", () => {
    const buf = new ArrayBuffer(-0.5);
    expect(buf.byteLength).toBe(0);
  });

  test("non-numeric string 'abc': ToNumber → NaN → 0", () => {
    const buf = new ArrayBuffer("abc");
    expect(buf.byteLength).toBe(0);
  });

  test("object {}: ToNumber → NaN → 0", () => {
    const buf = new ArrayBuffer({});
    expect(buf.byteLength).toBe(0);
  });

  test("numeric string is coerced to number", () => {
    const buf = new ArrayBuffer("8");
    expect(buf.byteLength).toBe(8);
  });

  test("true is coerced to 1", () => {
    const buf = new ArrayBuffer(true);
    expect(buf.byteLength).toBe(1);
  });

  test("false is coerced to 0", () => {
    const buf = new ArrayBuffer(false);
    expect(buf.byteLength).toBe(0);
  });

  test("null is coerced to 0", () => {
    const buf = new ArrayBuffer(null);
    expect(buf.byteLength).toBe(0);
  });

  test("undefined is coerced to 0", () => {
    const buf = new ArrayBuffer(undefined);
    expect(buf.byteLength).toBe(0);
  });

  test("float string '1.9' is truncated to 1", () => {
    const buf = new ArrayBuffer("1.9");
    expect(buf.byteLength).toBe(1);
  });

  test("negative integer -1 throws RangeError", () => {
    expect(() => new ArrayBuffer(-1)).toThrow(RangeError);
  });
});

describe("ArrayBuffer constructor meta", () => {
  test("byteLength cannot be changed (getter-only throws TypeError)", () => {
    const buf = new ArrayBuffer(8);
    expect(() => { buf.byteLength = 99; }).toThrow(TypeError);
    expect(buf.byteLength).toBe(8);
  });

  test("ArrayBuffer is not instanceof SharedArrayBuffer", () => {
    const buf = new ArrayBuffer(4);
    expect(buf instanceof SharedArrayBuffer).toBe(false);
  });

  test("throws TypeError when called without new", () => {
    expect(() => ArrayBuffer(8)).toThrow(TypeError);
  });

  test("throws TypeError when called without new and no arguments", () => {
    expect(() => ArrayBuffer()).toThrow(TypeError);
  });
});
