/*---
description: get ArrayBuffer.prototype.immutable
features: [ArrayBuffer, immutable-arraybuffer]
---*/

describe("get ArrayBuffer.prototype.immutable", () => {
  test("returns false for a new buffer", () => {
    expect(new ArrayBuffer(8).immutable).toBe(false);
  });

  test("returns false for a zero-length buffer", () => {
    expect(new ArrayBuffer(0).immutable).toBe(false);
  });

  test("returns false for a resizable buffer", () => {
    expect(new ArrayBuffer(4, { maxByteLength: 16 }).immutable).toBe(false);
  });

  test("returns true after transferToImmutable()", () => {
    expect(new ArrayBuffer(8).transferToImmutable().immutable).toBe(true);
  });

  test("is a configurable, non-enumerable accessor on the prototype", () => {
    const desc = Object.getOwnPropertyDescriptor(
      ArrayBuffer.prototype,
      "immutable",
    );
    expect(typeof desc.get).toBe("function");
    expect(desc.set).toBe(undefined);
    expect(desc.configurable).toBe(true);
    expect(desc.enumerable).toBe(false);
  });

  test("throws TypeError when called on non-ArrayBuffer", () => {
    const getter = Object.getOwnPropertyDescriptor(
      ArrayBuffer.prototype,
      "immutable",
    ).get;
    expect(() => getter.call({})).toThrow(TypeError);
  });
});
