/*---
description: ArrayBuffer constructor with options (maxByteLength)
features: [ArrayBuffer, resizable-arraybuffer]
---*/

describe("ArrayBuffer constructor with maxByteLength option", () => {
  test("creates a resizable buffer", () => {
    const buf = new ArrayBuffer(4, { maxByteLength: 16 });
    expect(buf.byteLength).toBe(4);
    expect(buf.maxByteLength).toBe(16);
    expect(buf.resizable).toBe(true);
  });

  test("maxByteLength equal to byteLength", () => {
    const buf = new ArrayBuffer(8, { maxByteLength: 8 });
    expect(buf.byteLength).toBe(8);
    expect(buf.maxByteLength).toBe(8);
    expect(buf.resizable).toBe(true);
  });

  test("maxByteLength of 0 with byteLength of 0", () => {
    const buf = new ArrayBuffer(0, { maxByteLength: 0 });
    expect(buf.byteLength).toBe(0);
    expect(buf.maxByteLength).toBe(0);
    expect(buf.resizable).toBe(true);
  });

  test("zero initial length with positive maxByteLength", () => {
    const buf = new ArrayBuffer(0, { maxByteLength: 64 });
    expect(buf.byteLength).toBe(0);
    expect(buf.maxByteLength).toBe(64);
    expect(buf.resizable).toBe(true);
  });

  test("without options produces fixed-length buffer", () => {
    const buf = new ArrayBuffer(8);
    expect(buf.resizable).toBe(false);
    expect(buf.maxByteLength).toBe(8);
  });

  test("throws RangeError when byteLength > maxByteLength", () => {
    expect(() => new ArrayBuffer(16, { maxByteLength: 8 })).toThrow(RangeError);
  });

  test("undefined maxByteLength in options creates fixed-length", () => {
    const buf = new ArrayBuffer(8, { maxByteLength: undefined });
    expect(buf.resizable).toBe(false);
  });

  test("empty options object creates fixed-length", () => {
    const buf = new ArrayBuffer(8, {});
    expect(buf.resizable).toBe(false);
  });

  test("undefined options creates fixed-length", () => {
    const buf = new ArrayBuffer(8, undefined);
    expect(buf.resizable).toBe(false);
  });

  test("detached is false for newly created resizable buffer", () => {
    const buf = new ArrayBuffer(4, { maxByteLength: 16 });
    expect(buf.detached).toBe(false);
  });

  test("throws RangeError for negative maxByteLength", () => {
    expect(() => new ArrayBuffer(0, { maxByteLength: -1 })).toThrow(RangeError);
  });

  test("primitive options are silently ignored per spec", () => {
    // ES2026 §25.1.3.7: If options is not an Object, return empty
    const buf1 = new ArrayBuffer(8, 42);
    expect(buf1.resizable).toBe(false);
    expect(buf1.byteLength).toBe(8);

    const buf2 = new ArrayBuffer(8, "hello");
    expect(buf2.resizable).toBe(false);

    const buf3 = new ArrayBuffer(8, true);
    expect(buf3.resizable).toBe(false);

    const buf4 = new ArrayBuffer(8, null);
    expect(buf4.resizable).toBe(false);
  });

  test("NaN maxByteLength is coerced to 0 via ToIndex", () => {
    const buf = new ArrayBuffer(0, { maxByteLength: NaN });
    expect(buf.resizable).toBe(true);
    expect(buf.maxByteLength).toBe(0);
  });

  test("fractional maxByteLength is truncated via ToIndex", () => {
    const buf = new ArrayBuffer(0, { maxByteLength: 8.9 });
    expect(buf.resizable).toBe(true);
    expect(buf.maxByteLength).toBe(8);
  });
});
