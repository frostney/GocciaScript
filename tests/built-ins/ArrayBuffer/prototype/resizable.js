/*---
description: ArrayBuffer.prototype.resizable
features: [ArrayBuffer, resizable-arraybuffer]
---*/

describe("get ArrayBuffer.prototype.resizable", () => {
  test("returns false for a fixed-length buffer", () => {
    const buf = new ArrayBuffer(8);
    expect(buf.resizable).toBe(false);
  });

  test("returns true for a resizable buffer", () => {
    const buf = new ArrayBuffer(4, { maxByteLength: 16 });
    expect(buf.resizable).toBe(true);
  });

  test("returns false for zero-length fixed buffer", () => {
    const buf = new ArrayBuffer(0);
    expect(buf.resizable).toBe(false);
  });

  test("returns true for resizable buffer with zero initial length", () => {
    const buf = new ArrayBuffer(0, { maxByteLength: 16 });
    expect(buf.resizable).toBe(true);
  });

  test("returns false for detached fixed-length buffer", () => {
    const buf = new ArrayBuffer(8);
    buf.transfer();
    expect(buf.resizable).toBe(false);
  });

  test("returns true for detached resizable buffer", () => {
    const buf = new ArrayBuffer(4, { maxByteLength: 16 });
    buf.transfer();
    expect(buf.resizable).toBe(true);
  });

  test("throws TypeError when called on non-ArrayBuffer", () => {
    const getter = Object.getOwnPropertyDescriptor(ArrayBuffer.prototype, "resizable").get;
    expect(() => getter.call({})).toThrow(TypeError);
  });
});
