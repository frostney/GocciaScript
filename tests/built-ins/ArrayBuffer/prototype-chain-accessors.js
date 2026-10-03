/*---
description: >
  byteLength, maxByteLength, resizable, detached and immutable are accessors on
  ArrayBuffer.prototype, so an ArrayBuffer answers them only through its
  prototype chain
features: [ArrayBuffer, resizable-arraybuffer, arraybuffer-transfer, immutable-arraybuffer, Reflect]
---*/

const NAMES = ["byteLength", "maxByteLength", "resizable", "detached", "immutable"];

describe("ArrayBuffer accessors through the prototype chain", () => {
  test("a null prototype or a prototype without the accessors gives undefined", () => {
    for (const prototype of [null, {}]) {
      const buffer = Object.setPrototypeOf(new ArrayBuffer(4, { maxByteLength: 8 }), prototype);
      for (const name of NAMES) {
        expect(buffer[name]).toBeUndefined();
        expect(Reflect.get(buffer, name)).toBeUndefined();
      }
    }
  });

  test("an immutable or detached buffer without the accessors gives undefined too", () => {
    const immutable = Object.setPrototypeOf(new ArrayBuffer(4).transferToImmutable(), null);
    const source = new ArrayBuffer(4);
    source.transfer();
    const detached = Object.setPrototypeOf(source, null);

    for (const name of NAMES) {
      expect(immutable[name]).toBeUndefined();
      expect(detached[name]).toBeUndefined();
    }
  });

  test("a prototype inheriting from ArrayBuffer.prototype gives the built-in values", () => {
    const resizable = Object.setPrototypeOf(new ArrayBuffer(4, { maxByteLength: 8 }),
      Object.create(ArrayBuffer.prototype));
    expect(resizable.byteLength).toBe(4);
    expect(resizable.maxByteLength).toBe(8);
    expect(resizable.resizable).toBe(true);
    expect(resizable.detached).toBe(false);
    expect(resizable.immutable).toBe(false);

    const detached = new ArrayBuffer(4);
    detached.transfer();
    expect(detached.byteLength).toBe(0);
    expect(detached.maxByteLength).toBe(0);
    expect(detached.detached).toBe(true);
    expect(new ArrayBuffer(2).transferToImmutable().immutable).toBe(true);
  });

  test("a data property, an own property or a getter found first is what is read", () => {
    const withData = Object.setPrototypeOf(new ArrayBuffer(4), { byteLength: "data" });
    expect(withData.byteLength).toBe("data");

    const withOwn = new ArrayBuffer(4);
    Object.defineProperty(withOwn, "maxByteLength", { value: "own" });
    expect(withOwn.maxByteLength).toBe("own");

    class Tagged extends ArrayBuffer {
      get resizable() { return ["subclass", this.byteLength]; }
    }
    expect(new Tagged(3).resizable).toEqual(["subclass", 3]);
  });

  test("a deleted accessor is no longer found", () => {
    const descriptor = Object.getOwnPropertyDescriptor(ArrayBuffer.prototype, "detached");
    delete ArrayBuffer.prototype.detached;
    try {
      expect(new ArrayBuffer(4).detached).toBeUndefined();
    } finally {
      Object.defineProperty(ArrayBuffer.prototype, "detached", descriptor);
    }
    expect(new ArrayBuffer(4).detached).toBe(false);
  });

  test("the built-in getter is called with the receiver Reflect.get is given", () => {
    const buffer = new ArrayBuffer(4);

    expect(Reflect.get(buffer, "byteLength", new ArrayBuffer(16))).toBe(16);
    expect(() => Reflect.get(buffer, "byteLength", {})).toThrow(TypeError);
    expect(() => Reflect.get(buffer, "byteLength", new SharedArrayBuffer(2))).toThrow(TypeError);
  });

  test("another built-in's getter found on the chain is called", () => {
    const buffer = Object.setPrototypeOf(new ArrayBuffer(4), Map.prototype);

    expect(() => buffer.size).toThrow(TypeError);
    expect(Object.getPrototypeOf(buffer)).toBe(Map.prototype);
  });

  test("a Proxy on the chain is asked through its get trap", () => {
    const keys = [];
    const prototype = new Proxy(ArrayBuffer.prototype, {
      get(target, key, receiver) {
        keys.push(key);
        return Reflect.get(target, key, receiver);
      },
    });
    const buffer = Object.setPrototypeOf(new ArrayBuffer(4), prototype);

    expect(buffer.byteLength).toBe(4);
    expect(buffer.resizable).toBe(false);
    expect(keys).toEqual(["byteLength", "resizable"]);
  });

  test("the SharedArrayBuffer getters reject an ArrayBuffer that inherits them", () => {
    const buffer = Object.setPrototypeOf(new ArrayBuffer(4), SharedArrayBuffer.prototype);

    expect(() => buffer.byteLength).toThrow(TypeError);
    expect(() => buffer.growable).toThrow(TypeError);
    expect(buffer.resizable).toBeUndefined();
  });
});
