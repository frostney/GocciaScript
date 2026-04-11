/*---
description: ArrayBuffer.prototype.maxByteLength
features: [ArrayBuffer, resizable-arraybuffer]
---*/

describe("get ArrayBuffer.prototype.maxByteLength", () => {
  test("returns byteLength for fixed-length buffer", () => {
    const buf = new ArrayBuffer(8);
    expect(buf.maxByteLength).toBe(8);
  });

  test("returns maxByteLength for resizable buffer", () => {
    const buf = new ArrayBuffer(4, { maxByteLength: 16 });
    expect(buf.maxByteLength).toBe(16);
  });

  test("returns 0 for zero-length fixed buffer", () => {
    const buf = new ArrayBuffer(0);
    expect(buf.maxByteLength).toBe(0);
  });

  test("returns 0 for detached buffer", () => {
    const buf = new ArrayBuffer(8);
    buf.transfer();
    expect(buf.maxByteLength).toBe(0);
  });

  test("returns 0 for detached resizable buffer", () => {
    const buf = new ArrayBuffer(4, { maxByteLength: 16 });
    buf.transfer();
    expect(buf.maxByteLength).toBe(0);
  });

  test("reflects current byteLength for fixed buffers", () => {
    const buf = new ArrayBuffer(8);
    expect(buf.maxByteLength).toBe(8);
  });

  test("does not change after resize", () => {
    const buf = new ArrayBuffer(4, { maxByteLength: 16 });
    buf.resize(8);
    expect(buf.maxByteLength).toBe(16);
    buf.resize(2);
    expect(buf.maxByteLength).toBe(16);
  });

  test("throws TypeError when called on non-ArrayBuffer", () => {
    const getter = Object.getOwnPropertyDescriptor(ArrayBuffer.prototype, "maxByteLength").get;
    expect(() => getter.call({})).toThrow(TypeError);
  });
});
