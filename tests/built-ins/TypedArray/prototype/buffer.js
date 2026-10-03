/*---
description: >
  %TypedArray%.prototype.buffer is an accessor that a typed array inherits, so
  a read of `buffer` is an ordinary property lookup
features: [TypedArray]
---*/

const TypedArrayPrototype = Object.getPrototypeOf(Uint8Array.prototype);
const bufferDescriptor = Object.getOwnPropertyDescriptor(TypedArrayPrototype, "buffer");

describe("TypedArray.prototype.buffer", () => {
  test("is an accessor without a setter on %TypedArray%.prototype", () => {
    expect(typeof bufferDescriptor.get).toBe("function");
    expect(bufferDescriptor.set).toBeUndefined();
    expect(bufferDescriptor.enumerable).toBe(false);
    expect(bufferDescriptor.configurable).toBe(true);
    expect(bufferDescriptor.get.name).toBe("get buffer");
    expect(Object.hasOwn(Uint8Array.prototype, "buffer")).toBe(false);
  });

  test("is not an own property of a typed array", () => {
    const ta = new Uint8Array(4);

    expect(Object.hasOwn(ta, "buffer")).toBe(false);
    expect(Object.getOwnPropertyDescriptor(ta, "buffer")).toBeUndefined();
    expect("buffer" in ta).toBe(true);
  });

  test("returns the buffer the view was created over", () => {
    const buffer = new ArrayBuffer(8);
    const shared = new SharedArrayBuffer(8);

    expect(new Uint8Array(buffer).buffer).toBe(buffer);
    expect(new Int32Array(buffer, 4).buffer).toBe(buffer);
    expect(new Uint8Array(shared).buffer).toBe(shared);
    expect(new Uint8Array(4).buffer instanceof ArrayBuffer).toBe(true);
  });

  test("the getter throws a TypeError for a receiver that is not a typed array", () => {
    const get = bufferDescriptor.get;
    const buffer = new ArrayBuffer(8);

    expect(get.call(new Uint8Array(buffer))).toBe(buffer);
    expect(() => get.call({})).toThrow(TypeError);
    expect(() => get.call(new DataView(buffer))).toThrow(TypeError);
    expect(() => TypedArrayPrototype.buffer).toThrow(TypeError);
  });

  test("still returns the buffer after it is detached", () => {
    const buffer = new ArrayBuffer(8);
    const ta = new Uint8Array(buffer);

    buffer.transfer();

    expect(ta.buffer).toBe(buffer);
  });

  test("a replaced prototype answers the read", () => {
    const ta = Object.setPrototypeOf(new Uint8Array(4), { buffer: "b" });

    expect(ta.buffer).toBe("b");
    expect(Reflect.get(ta, "buffer")).toBe("b");
    expect(Object.hasOwn(ta, "buffer")).toBe(false);
  });

  test("a typed array without a prototype has no buffer property", () => {
    const buffer = new ArrayBuffer(4);
    const ta = Object.setPrototypeOf(new Uint8Array(buffer), null);

    expect(ta.buffer).toBeUndefined();
    expect("buffer" in ta).toBe(false);
    expect(bufferDescriptor.get.call(ta)).toBe(buffer);
  });

  test("a subclass getter overrides it and reaches it through super", () => {
    class Hidden extends Uint8Array {
      get buffer() {
        return "hidden";
      }
    }
    class Wrapped extends Uint8Array {
      get buffer() {
        return [super.buffer];
      }
    }
    const buffer = new ArrayBuffer(4);

    expect(new Hidden(4).buffer).toBe("hidden");
    expect(new Wrapped(buffer).buffer[0]).toBe(buffer);
  });

  test("an own property shadows it until it is deleted", () => {
    const buffer = new ArrayBuffer(4);
    const ta = new Uint8Array(buffer);

    Object.defineProperty(ta, "buffer", { value: "own", configurable: true });
    expect(ta.buffer).toBe("own");
    expect(Object.hasOwn(ta, "buffer")).toBe(true);

    expect(delete ta.buffer).toBe(true);
    expect(ta.buffer).toBe(buffer);
  });

  test("a redefinition on %TypedArray%.prototype is observed until it is restored", () => {
    const buffer = new ArrayBuffer(4);
    const ta = new Uint8Array(buffer);

    try {
      Object.defineProperty(TypedArrayPrototype, "buffer", {
        get() {
          return this === ta ? "redefined" : "wrong receiver";
        },
        configurable: true,
      });
      expect(ta.buffer).toBe("redefined");

      delete TypedArrayPrototype.buffer;
      expect(ta.buffer).toBeUndefined();
    } finally {
      Object.defineProperty(TypedArrayPrototype, "buffer", bufferDescriptor);
    }

    expect(ta.buffer).toBe(buffer);
  });
});
