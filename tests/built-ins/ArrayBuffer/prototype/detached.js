/*---
description: ArrayBuffer.prototype.detached
features: [ArrayBuffer, arraybuffer-transfer]
---*/

describe("get ArrayBuffer.prototype.detached", () => {
  test("returns false for a new buffer", () => {
    const buf = new ArrayBuffer(8);
    expect(buf.detached).toBe(false);
  });

  test("returns false for a zero-length buffer", () => {
    const buf = new ArrayBuffer(0);
    expect(buf.detached).toBe(false);
  });

  test("returns true after transfer()", () => {
    const buf = new ArrayBuffer(8);
    buf.transfer();
    expect(buf.detached).toBe(true);
  });

  test("returns true after transferToFixedLength()", () => {
    const buf = new ArrayBuffer(8);
    buf.transferToFixedLength();
    expect(buf.detached).toBe(true);
  });

  test("returns false for resizable buffer", () => {
    const buf = new ArrayBuffer(4, { maxByteLength: 16 });
    expect(buf.detached).toBe(false);
  });

  test("returns true for detached resizable buffer", () => {
    const buf = new ArrayBuffer(4, { maxByteLength: 16 });
    buf.transfer();
    expect(buf.detached).toBe(true);
  });

  test("throws TypeError when called on non-ArrayBuffer", () => {
    const getter = Object.getOwnPropertyDescriptor(ArrayBuffer.prototype, "detached").get;
    expect(() => getter.call({})).toThrow(TypeError);
  });
});
