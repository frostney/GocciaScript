/*---
description: >
  %TypedArray%.prototype.length is an accessor that a typed array inherits, so
  a read of `length` is an ordinary property lookup
features: [TypedArray, resizable-arraybuffer]
---*/

const TypedArrayPrototype = Object.getPrototypeOf(Uint8Array.prototype);
const lengthDescriptor = Object.getOwnPropertyDescriptor(TypedArrayPrototype, "length");

describe("TypedArray.prototype.length", () => {
  test("is an accessor without a setter on %TypedArray%.prototype", () => {
    expect(typeof lengthDescriptor.get).toBe("function");
    expect(lengthDescriptor.set).toBeUndefined();
    expect(lengthDescriptor.enumerable).toBe(false);
    expect(lengthDescriptor.configurable).toBe(true);
    expect(lengthDescriptor.get.name).toBe("get length");
    expect(Object.hasOwn(Uint8Array.prototype, "length")).toBe(false);
  });

  test("is not an own property of a typed array", () => {
    const ta = new Uint8Array(4);

    expect(Object.hasOwn(ta, "length")).toBe(false);
    expect(ta.hasOwnProperty("length")).toBe(false);
    expect(Object.getOwnPropertyDescriptor(ta, "length")).toBeUndefined();
    expect(Reflect.ownKeys(ta)).toEqual(["0", "1", "2", "3"]);
    expect("length" in ta).toBe(true);
  });

  test("returns the element count of the view", () => {
    expect(new Uint8Array(4).length).toBe(4);
    expect(new Float64Array(0).length).toBe(0);
    expect(new Int16Array(new ArrayBuffer(16), 4).length).toBe(6);
    expect(new Int16Array(new ArrayBuffer(16), 4, 2).length).toBe(2);
  });

  test("the getter throws a TypeError for a receiver that is not a typed array", () => {
    const get = lengthDescriptor.get;

    expect(get.call(new Float32Array(3))).toBe(3);
    expect(() => get.call({})).toThrow(TypeError);
    expect(() => get.call([1, 2])).toThrow(TypeError);
    expect(() => get.call(new DataView(new ArrayBuffer(4)))).toThrow(TypeError);
    expect(() => get.call(undefined)).toThrow(TypeError);
    expect(() => TypedArrayPrototype.length).toThrow(TypeError);
    expect(() => Uint8Array.prototype.length).toThrow(TypeError);
  });

  test("follows a resizable buffer and is 0 while the view is out of bounds", () => {
    const buffer = new ArrayBuffer(8, { maxByteLength: 16 });
    const fixed = new Uint8Array(buffer, 4, 4);
    const tracking = new Uint16Array(buffer, 4);

    expect(fixed.length).toBe(4);
    expect(tracking.length).toBe(2);

    buffer.resize(16);
    expect(fixed.length).toBe(4);
    expect(tracking.length).toBe(6);

    buffer.resize(7);
    expect(fixed.length).toBe(0);
    expect(tracking.length).toBe(1);

    buffer.resize(3);
    expect(fixed.length).toBe(0);
    expect(tracking.length).toBe(0);

    buffer.resize(8);
    expect(fixed.length).toBe(4);
    expect(tracking.length).toBe(2);
  });

  test("is 0 once the buffer is detached", () => {
    const buffer = new ArrayBuffer(8);
    const ta = new Uint16Array(buffer, 2, 2);

    buffer.transfer();

    expect(ta.length).toBe(0);
  });

  test("a replaced prototype answers the read", () => {
    const ta = Object.setPrototypeOf(new Uint8Array(4), { length: "x" });
    const key = "len" + "gth";

    expect(ta.length).toBe("x");
    expect(ta[key]).toBe("x");
    expect(Reflect.get(ta, "length")).toBe("x");
    expect(Object.hasOwn(ta, "length")).toBe(false);
    expect(ta[3]).toBe(0);
  });

  test("a typed array without a prototype has no length", () => {
    const ta = Object.setPrototypeOf(new Uint8Array(4), null);

    expect(ta.length).toBeUndefined();
    expect("length" in ta).toBe(false);
    expect(ta[3]).toBe(0);
    expect(ta[4]).toBeUndefined();
    expect(lengthDescriptor.get.call(ta)).toBe(4);
  });

  test("a subclass getter overrides it and reaches it through super", () => {
    class Fixed extends Uint8Array {
      get length() {
        return 99;
      }
    }
    class Doubled extends Uint8Array {
      get length() {
        return super.length * 2;
      }
    }
    class Inherited extends Uint8Array {}

    expect(new Fixed(4).length).toBe(99);
    expect(new Doubled(4).length).toBe(8);
    expect(new Inherited(4).length).toBe(4);
    expect(Object.hasOwn(new Inherited(4), "length")).toBe(false);
  });

  test("an own property shadows it until it is deleted", () => {
    const ta = new Uint8Array(4);

    Object.defineProperty(ta, "length", { value: 123, configurable: true });
    expect(ta.length).toBe(123);
    expect(Object.hasOwn(ta, "length")).toBe(true);

    Object.defineProperty(ta, "length", {
      get() {
        return this === ta ? "own getter" : "wrong receiver";
      },
      configurable: true,
    });
    expect(ta.length).toBe("own getter");

    expect(delete ta.length).toBe(true);
    expect(ta.length).toBe(4);
    expect(Object.hasOwn(ta, "length")).toBe(false);
  });

  test("a typed array with other own properties still inherits it", () => {
    const ta = new Uint8Array(4);
    ta.label = "samples";

    expect(ta.length).toBe(4);
    expect(ta.label).toBe("samples");
  });

  test("a redefinition on %TypedArray%.prototype is observed by every typed array", () => {
    const bytes = new Uint8Array(4);
    const floats = new Float64Array(3);

    try {
      Object.defineProperty(TypedArrayPrototype, "length", {
        get() {
          return this === bytes ? "bytes" : "other";
        },
        configurable: true,
      });
      expect(bytes.length).toBe("bytes");
      expect(floats.length).toBe("other");

      Object.defineProperty(TypedArrayPrototype, "length", { value: 5, configurable: true });
      expect(bytes.length).toBe(5);
      expect(floats.length).toBe(5);

      Object.defineProperty(TypedArrayPrototype, "length", { get: undefined, configurable: true });
      expect(bytes.length).toBeUndefined();

      delete TypedArrayPrototype.length;
      expect(bytes.length).toBeUndefined();
      expect("length" in bytes).toBe(false);

      Object.prototype.length = "from Object.prototype";
      expect(bytes.length).toBe("from Object.prototype");
      expect(floats.length).toBe("from Object.prototype");
    } finally {
      delete Object.prototype.length;
      Object.defineProperty(TypedArrayPrototype, "length", lengthDescriptor);
    }

    expect(bytes.length).toBe(4);
    expect(floats.length).toBe(3);
  });

  test("a property on a constructor's prototype is observed by that kind only", () => {
    const shorts = new Uint16Array(4);
    const floats = new Float64Array(3);

    try {
      Object.defineProperty(Uint16Array.prototype, "length", {
        get() {
          return 1000 + lengthDescriptor.get.call(this);
        },
        configurable: true,
      });
      expect(shorts.length).toBe(1004);
      expect(floats.length).toBe(3);
    } finally {
      delete Uint16Array.prototype.length;
    }

    expect(shorts.length).toBe(4);
  });

  test("a change to the prototype of a constructor's prototype is observed", () => {
    const shorts = new Uint16Array(4);
    const floats = new Float64Array(3);

    try {
      Object.setPrototypeOf(Uint16Array.prototype, null);
      expect(shorts.length).toBeUndefined();
      expect(floats.length).toBe(3);

      Object.setPrototypeOf(Uint16Array.prototype, { length: "detour" });
      expect(shorts.length).toBe("detour");
    } finally {
      Object.setPrototypeOf(Uint16Array.prototype, TypedArrayPrototype);
    }

    expect(shorts.length).toBe(4);
  });

  test("a read repeated at one place observes a prototype change between reads", () => {
    const ta = new Uint8Array(4);
    const seen = [];

    for (const step of [0, 1, 2, 3, 4, 5]) {
      if (step === 2) Object.setPrototypeOf(ta, { length: "replaced" });
      if (step === 4) Object.setPrototypeOf(ta, Uint8Array.prototype);
      seen.push(ta.length);
    }

    expect(seen).toEqual([4, 4, "replaced", "replaced", 4, 4]);
  });

  test("the getter keeps its meaning under another name and on another object", () => {
    const holder = {};
    Object.defineProperty(holder, "size", lengthDescriptor);
    const ta = Object.setPrototypeOf(new Uint16Array(4), holder);

    expect(ta.size).toBe(4);
    expect(ta.length).toBeUndefined();
    expect(() => Object.create(holder).size).toThrow(TypeError);
  });

  test("a Proxy prototype receives the read with the typed array as receiver", () => {
    const seen = [];
    const ta = new Uint8Array(4);
    const proxy = new Proxy(Uint8Array.prototype, {
      get(target, key, receiver) {
        seen.push(key, receiver === ta);
        return Reflect.get(target, key, receiver);
      },
    });
    Object.setPrototypeOf(ta, proxy);

    expect(ta.length).toBe(4);
    expect(seen).toEqual(["length", true]);
  });

  test("a typed array used as a prototype passes the read on with the original receiver", () => {
    const ta = Object.setPrototypeOf(new Uint8Array(4), new Float64Array(9));

    expect(ta.length).toBe(4);
    expect(() => Object.create(new Uint8Array(4)).length).toThrow(TypeError);
  });

  test("Reflect.get reads it for the receiver it is given", () => {
    const ta = new Uint8Array(4);

    expect(Reflect.get(ta, "length")).toBe(4);
    expect(Reflect.get(ta, "length", new Float64Array(9))).toBe(9);
    expect(() => Reflect.get(ta, "length", {})).toThrow(TypeError);
  });
});
