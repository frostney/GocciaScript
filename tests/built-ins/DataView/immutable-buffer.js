/*---
description: DataView writes over an immutable ArrayBuffer
features: [DataView, ArrayBuffer, immutable-arraybuffer]
---*/

const immutableView = () => {
  const buf = new ArrayBuffer(8);
  new DataView(buf).setUint32(0, 0x01020304);
  return new DataView(buf.transferToImmutable());
};

describe("DataView over an immutable ArrayBuffer", () => {
  test("reads work normally", () => {
    expect(immutableView().getUint32(0)).toBe(0x01020304);
  });

  test("setUint8 throws a TypeError", () => {
    expect(() => immutableView().setUint8(0, 9)).toThrow(TypeError);
  });

  test("setUint32 throws a TypeError", () => {
    expect(() => immutableView().setUint32(0, 9)).toThrow(TypeError);
  });

  test("setFloat64 throws a TypeError", () => {
    expect(() => immutableView().setFloat64(0, 1.5)).toThrow(TypeError);
  });

  test("setBigInt64 throws a TypeError", () => {
    expect(() => immutableView().setBigInt64(0, 9n)).toThrow(TypeError);
  });

  test("a rejected write leaves the bytes unchanged", () => {
    const dv = immutableView();
    expect(() => dv.setUint32(0, 0xffffffff)).toThrow(TypeError);
    expect(dv.getUint32(0)).toBe(0x01020304);
  });
});
