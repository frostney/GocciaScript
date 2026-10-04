/*---
description: >
  %TypedArray%.prototype.byteOffset is an accessor that a typed array
  inherits, so a read of `byteOffset` is an ordinary property lookup
features: [TypedArray, resizable-arraybuffer]
---*/

const TypedArrayPrototype = Object.getPrototypeOf(Uint8Array.prototype);
const byteOffsetDescriptor = Object.getOwnPropertyDescriptor(TypedArrayPrototype, "byteOffset");

describe("TypedArray.prototype.byteOffset", () => {
  test("is an accessor without a setter on %TypedArray%.prototype", () => {
    expect(typeof byteOffsetDescriptor.get).toBe("function");
    expect(byteOffsetDescriptor.set).toBeUndefined();
    expect(byteOffsetDescriptor.enumerable).toBe(false);
    expect(byteOffsetDescriptor.configurable).toBe(true);
    expect(byteOffsetDescriptor.get.name).toBe("get byteOffset");
    expect(Object.hasOwn(Uint8Array.prototype, "byteOffset")).toBe(false);
  });

  test("is not an own property of a typed array", () => {
    const ta = new Uint16Array(new ArrayBuffer(8), 2);

    expect(Object.hasOwn(ta, "byteOffset")).toBe(false);
    expect(Object.getOwnPropertyDescriptor(ta, "byteOffset")).toBeUndefined();
    expect("byteOffset" in ta).toBe(true);
  });

  test("returns the offset of the view into its buffer", () => {
    expect(new Uint8Array(4).byteOffset).toBe(0);
    expect(new Uint16Array(new ArrayBuffer(8), 2).byteOffset).toBe(2);
    expect(new Float64Array(new ArrayBuffer(32), 16, 1).byteOffset).toBe(16);
    expect(new Uint8Array([1, 2, 3, 4]).subarray(3).byteOffset).toBe(3);
  });

  test("the getter throws a TypeError for a receiver that is not a typed array", () => {
    const get = byteOffsetDescriptor.get;

    expect(get.call(new Uint16Array(new ArrayBuffer(8), 6))).toBe(6);
    expect(() => get.call({})).toThrow(TypeError);
    expect(() => get.call(new DataView(new ArrayBuffer(4), 2))).toThrow(TypeError);
    expect(() => TypedArrayPrototype.byteOffset).toThrow(TypeError);
  });

  test("is 0 while the view is out of bounds and returns when it is back in bounds", () => {
    const buffer = new ArrayBuffer(8, { maxByteLength: 16 });
    const fixed = new Uint8Array(buffer, 4, 4);
    const tracking = new Uint16Array(buffer, 4);

    expect(fixed.byteOffset).toBe(4);
    expect(tracking.byteOffset).toBe(4);

    buffer.resize(7);
    expect(fixed.byteOffset).toBe(0);
    expect(tracking.byteOffset).toBe(4);

    buffer.resize(3);
    expect(fixed.byteOffset).toBe(0);
    expect(tracking.byteOffset).toBe(0);

    buffer.resize(8);
    expect(fixed.byteOffset).toBe(4);
    expect(tracking.byteOffset).toBe(4);
  });

  test("is 0 once the buffer is detached", () => {
    const buffer = new ArrayBuffer(8);
    const ta = new Uint16Array(buffer, 2, 2);

    buffer.transfer();

    expect(ta.byteOffset).toBe(0);
  });

  test("a replaced prototype answers the read", () => {
    const ta = Object.setPrototypeOf(new Uint16Array(new ArrayBuffer(8), 2), { byteOffset: "z" });

    expect(ta.byteOffset).toBe("z");
    expect(Reflect.get(ta, "byteOffset")).toBe("z");
    expect(Object.hasOwn(ta, "byteOffset")).toBe(false);
  });

  test("a typed array without a prototype has no byteOffset", () => {
    const ta = Object.setPrototypeOf(new Uint16Array(new ArrayBuffer(8), 2), null);

    expect(ta.byteOffset).toBeUndefined();
    expect("byteOffset" in ta).toBe(false);
    expect(byteOffsetDescriptor.get.call(ta)).toBe(2);
  });

  test("a subclass getter overrides it and reaches it through super", () => {
    class Fixed extends Uint16Array {
      get byteOffset() {
        return 97;
      }
    }
    class Shifted extends Uint16Array {
      get byteOffset() {
        return super.byteOffset + 100;
      }
    }

    expect(new Fixed(new ArrayBuffer(8), 2).byteOffset).toBe(97);
    expect(new Shifted(new ArrayBuffer(8), 2).byteOffset).toBe(102);
  });

  test("an own property shadows it until it is deleted", () => {
    const ta = new Uint16Array(new ArrayBuffer(8), 2);

    Object.defineProperty(ta, "byteOffset", { value: "own", configurable: true });
    expect(ta.byteOffset).toBe("own");
    expect(Object.hasOwn(ta, "byteOffset")).toBe(true);

    expect(delete ta.byteOffset).toBe(true);
    expect(ta.byteOffset).toBe(2);
  });

  test("a redefinition on %TypedArray%.prototype is observed until it is restored", () => {
    const ta = new Uint16Array(new ArrayBuffer(8), 2);

    try {
      Object.defineProperty(TypedArrayPrototype, "byteOffset", {
        get() {
          return this === ta ? "redefined" : "wrong receiver";
        },
        configurable: true,
      });
      expect(ta.byteOffset).toBe("redefined");

      delete TypedArrayPrototype.byteOffset;
      expect(ta.byteOffset).toBeUndefined();
    } finally {
      Object.defineProperty(TypedArrayPrototype, "byteOffset", byteOffsetDescriptor);
    }

    expect(ta.byteOffset).toBe(2);
  });
});
