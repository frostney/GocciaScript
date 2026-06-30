/*---
description: TypedArray views over an immutable ArrayBuffer
features: [TypedArray, ArrayBuffer, immutable-arraybuffer]
---*/

const immutableU8 = () => {
  const buf = new ArrayBuffer(4);
  const seed = new Uint8Array(buf);
  seed[0] = 10;
  seed[1] = 20;
  seed[2] = 30;
  seed[3] = 40;
  return new Uint8Array(buf.transferToImmutable());
};

describe("TypedArray over an immutable ArrayBuffer", () => {
  test("reads work normally", () => {
    const u8 = immutableU8();
    expect(u8[0]).toBe(10);
    expect(u8[3]).toBe(40);
    expect(u8.length).toBe(4);
  });

  test("indexed writes are silently ignored", () => {
    const u8 = immutableU8();
    u8[0] = 99;
    expect(u8[0]).toBe(10);
  });

  test("coerces the value before skipping the immutable write", () => {
    const u8 = immutableU8();
    let coerced = false;
    u8[0] = {
      valueOf() {
        coerced = true;
        return 99;
      },
    };
    expect(coerced).toBe(true);
    expect(u8[0]).toBe(10);
  });

  test("fill throws a TypeError", () => {
    expect(() => immutableU8().fill(0)).toThrow(TypeError);
  });

  test("set throws a TypeError", () => {
    expect(() => immutableU8().set([1, 2])).toThrow(TypeError);
  });

  test("sort throws a TypeError", () => {
    expect(() => immutableU8().sort()).toThrow(TypeError);
  });

  test("copyWithin throws a TypeError", () => {
    expect(() => immutableU8().copyWithin(0, 2)).toThrow(TypeError);
  });

  test("Object.defineProperty on an index throws a TypeError", () => {
    const u8 = immutableU8();
    expect(() => Object.defineProperty(u8, "0", { value: 99 })).toThrow(
      TypeError,
    );
    expect(u8[0]).toBe(10);
  });

  test("Reflect.defineProperty on an index returns false", () => {
    const u8 = immutableU8();
    expect(Reflect.defineProperty(u8, "0", { value: 99 })).toBe(false);
    expect(u8[0]).toBe(10);
  });
});
