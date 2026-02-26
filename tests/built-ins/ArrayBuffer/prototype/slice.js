/*---
description: ArrayBuffer.prototype.slice
features: [ArrayBuffer]
---*/

describe("ArrayBuffer.prototype.slice", () => {
  test("returns a new ArrayBuffer", () => {
    const buf = new ArrayBuffer(8);
    const sliced = buf.slice(0, 4);
    expect(sliced instanceof ArrayBuffer).toBe(true);
    expect(sliced).not.toBe(buf);
  });

  test("sliced buffer has correct byteLength", () => {
    const buf = new ArrayBuffer(16);
    const sliced = buf.slice(4, 12);
    expect(sliced.byteLength).toBe(8);
  });

  test("slice with no arguments copies the entire buffer", () => {
    const buf = new ArrayBuffer(8);
    const copy = buf.slice();
    expect(copy.byteLength).toBe(8);
    expect(copy).not.toBe(buf);
  });

  test("slice with only start copies to end", () => {
    const buf = new ArrayBuffer(10);
    const sliced = buf.slice(3);
    expect(sliced.byteLength).toBe(7);
  });

  test("negative start counts from end", () => {
    const buf = new ArrayBuffer(10);
    const sliced = buf.slice(-3);
    expect(sliced.byteLength).toBe(3);
  });

  test("negative end counts from end", () => {
    const buf = new ArrayBuffer(10);
    const sliced = buf.slice(2, -2);
    expect(sliced.byteLength).toBe(6);
  });

  test("both negative indices", () => {
    const buf = new ArrayBuffer(10);
    const sliced = buf.slice(-5, -2);
    expect(sliced.byteLength).toBe(3);
  });

  test("empty slice when start equals end", () => {
    const buf = new ArrayBuffer(8);
    const sliced = buf.slice(4, 4);
    expect(sliced.byteLength).toBe(0);
  });

  test("empty slice when start is greater than end", () => {
    const buf = new ArrayBuffer(8);
    const sliced = buf.slice(6, 2);
    expect(sliced.byteLength).toBe(0);
  });

  test("clamps start to 0 when negative exceeds length", () => {
    const buf = new ArrayBuffer(8);
    const sliced = buf.slice(-100, 4);
    expect(sliced.byteLength).toBe(4);
  });

  test("clamps end to byteLength when exceeding", () => {
    const buf = new ArrayBuffer(8);
    const sliced = buf.slice(2, 100);
    expect(sliced.byteLength).toBe(6);
  });

  test("clamps both out-of-range indices", () => {
    const buf = new ArrayBuffer(8);
    const sliced = buf.slice(-100, 100);
    expect(sliced.byteLength).toBe(8);
  });

  test("slice of zero-length buffer", () => {
    const buf = new ArrayBuffer(0);
    const sliced = buf.slice(0, 0);
    expect(sliced.byteLength).toBe(0);
  });
});

describe("ArrayBuffer.prototype.slice edge cases", () => {
  test("undefined end defaults to byteLength", () => {
    const buf = new ArrayBuffer(8);
    expect(buf.slice(0, undefined).byteLength).toBe(8);
    expect(buf.slice(2, undefined).byteLength).toBe(6);
  });

  test("undefined start defaults to 0", () => {
    const buf = new ArrayBuffer(8);
    expect(buf.slice(undefined).byteLength).toBe(8);
    expect(buf.slice(undefined, 4).byteLength).toBe(4);
  });

  test("NaN start is treated as 0", () => {
    const buf = new ArrayBuffer(8);
    expect(buf.slice(NaN).byteLength).toBe(8);
    expect(buf.slice(NaN, 4).byteLength).toBe(4);
  });

  test("NaN end is treated as 0", () => {
    const buf = new ArrayBuffer(8);
    expect(buf.slice(0, NaN).byteLength).toBe(0);
  });

  test("both NaN gives empty buffer", () => {
    const buf = new ArrayBuffer(8);
    expect(buf.slice(NaN, NaN).byteLength).toBe(0);
  });

  test("result is always a new instance", () => {
    const buf = new ArrayBuffer(8);
    const full = buf.slice();
    expect(full).not.toBe(buf);
    expect(full instanceof ArrayBuffer).toBe(true);
  });

  test("result is not a SharedArrayBuffer", () => {
    const buf = new ArrayBuffer(8);
    const sliced = buf.slice(0, 4);
    expect(sliced instanceof SharedArrayBuffer).toBe(false);
  });
});
