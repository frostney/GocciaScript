/*---
description: >
  %TypedArray%.prototype.byteLength is an accessor that a typed array
  inherits, so a read of `byteLength` is an ordinary property lookup
features: [TypedArray, resizable-arraybuffer]
---*/

const TypedArrayPrototype = Object.getPrototypeOf(Uint8Array.prototype);
const byteLengthDescriptor = Object.getOwnPropertyDescriptor(TypedArrayPrototype, "byteLength");

describe("TypedArray.prototype.byteLength", () => {
  test("is an accessor without a setter on %TypedArray%.prototype", () => {
    expect(typeof byteLengthDescriptor.get).toBe("function");
    expect(byteLengthDescriptor.set).toBeUndefined();
    expect(byteLengthDescriptor.enumerable).toBe(false);
    expect(byteLengthDescriptor.configurable).toBe(true);
    expect(byteLengthDescriptor.get.name).toBe("get byteLength");
    expect(Object.hasOwn(Uint8Array.prototype, "byteLength")).toBe(false);
  });

  test("is not an own property of a typed array", () => {
    const ta = new Uint32Array(2);

    expect(Object.hasOwn(ta, "byteLength")).toBe(false);
    expect(Object.getOwnPropertyDescriptor(ta, "byteLength")).toBeUndefined();
    expect("byteLength" in ta).toBe(true);
  });

  test("returns the number of bytes the view covers", () => {
    expect(new Uint8Array(4).byteLength).toBe(4);
    expect(new Float64Array(3).byteLength).toBe(24);
    expect(new BigInt64Array(2).byteLength).toBe(16);
    expect(new Int16Array(new ArrayBuffer(16), 4).byteLength).toBe(12);
    expect(new Int16Array(new ArrayBuffer(16), 4, 2).byteLength).toBe(4);
  });

  test("the getter throws a TypeError for a receiver that is not a typed array", () => {
    const get = byteLengthDescriptor.get;

    expect(get.call(new Float32Array(3))).toBe(12);
    expect(() => get.call({})).toThrow(TypeError);
    expect(() => get.call(new ArrayBuffer(4))).toThrow(TypeError);
    expect(() => TypedArrayPrototype.byteLength).toThrow(TypeError);
  });

  test("follows a resizable buffer and is 0 while the view is out of bounds", () => {
    const buffer = new ArrayBuffer(8, { maxByteLength: 16 });
    const fixed = new Uint8Array(buffer, 4, 4);
    const tracking = new Uint16Array(buffer, 4);

    expect(fixed.byteLength).toBe(4);
    expect(tracking.byteLength).toBe(4);

    buffer.resize(16);
    expect(fixed.byteLength).toBe(4);
    expect(tracking.byteLength).toBe(12);

    buffer.resize(7);
    expect(fixed.byteLength).toBe(0);
    expect(tracking.byteLength).toBe(2);

    buffer.resize(3);
    expect(fixed.byteLength).toBe(0);
    expect(tracking.byteLength).toBe(0);
  });

  test("is 0 once the buffer is detached", () => {
    const buffer = new ArrayBuffer(8);
    const ta = new Uint16Array(buffer, 2, 2);

    buffer.transfer();

    expect(ta.byteLength).toBe(0);
  });

  test("a replaced prototype answers the read", () => {
    const ta = Object.setPrototypeOf(new Uint32Array(2), { byteLength: "y" });

    expect(ta.byteLength).toBe("y");
    expect(Reflect.get(ta, "byteLength")).toBe("y");
    expect(Object.hasOwn(ta, "byteLength")).toBe(false);
  });

  test("a typed array without a prototype has no byteLength", () => {
    const ta = Object.setPrototypeOf(new Uint32Array(2), null);

    expect(ta.byteLength).toBeUndefined();
    expect("byteLength" in ta).toBe(false);
    expect(byteLengthDescriptor.get.call(ta)).toBe(8);
  });

  test("a subclass getter overrides it and reaches it through super", () => {
    class Fixed extends Uint32Array {
      get byteLength() {
        return 98;
      }
    }
    class Padded extends Uint32Array {
      get byteLength() {
        return super.byteLength + 1;
      }
    }

    expect(new Fixed(2).byteLength).toBe(98);
    expect(new Padded(2).byteLength).toBe(9);
  });

  test("an own property shadows it until it is deleted", () => {
    const ta = new Uint32Array(2);

    Object.defineProperty(ta, "byteLength", { value: "own", configurable: true });
    expect(ta.byteLength).toBe("own");
    expect(Object.hasOwn(ta, "byteLength")).toBe(true);

    expect(delete ta.byteLength).toBe(true);
    expect(ta.byteLength).toBe(8);
  });

  test("a redefinition on %TypedArray%.prototype is observed until it is restored", () => {
    const ta = new Uint32Array(2);

    try {
      Object.defineProperty(TypedArrayPrototype, "byteLength", {
        get() {
          return this === ta ? "redefined" : "wrong receiver";
        },
        configurable: true,
      });
      expect(ta.byteLength).toBe("redefined");
      expect(ta.length).toBe(2);

      delete TypedArrayPrototype.byteLength;
      expect(ta.byteLength).toBeUndefined();
    } finally {
      Object.defineProperty(TypedArrayPrototype, "byteLength", byteLengthDescriptor);
    }

    expect(ta.byteLength).toBe(8);
  });

  test("the getter installed under `length` answers reads of length", () => {
    const ta = new Uint32Array(2);
    const lengthDescriptor = Object.getOwnPropertyDescriptor(TypedArrayPrototype, "length");

    try {
      Object.defineProperty(TypedArrayPrototype, "length", {
        get: byteLengthDescriptor.get,
        configurable: true,
      });
      expect(ta.length).toBe(8);
    } finally {
      Object.defineProperty(TypedArrayPrototype, "length", lengthDescriptor);
    }

    expect(ta.length).toBe(2);
  });
});
