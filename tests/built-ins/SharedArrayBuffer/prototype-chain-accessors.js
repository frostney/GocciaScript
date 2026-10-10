/*---
description: >
  byteLength, maxByteLength and growable are accessors on
  SharedArrayBuffer.prototype, so a SharedArrayBuffer answers them only through
  its prototype chain
features: [SharedArrayBuffer, resizable-arraybuffer, Reflect]
---*/

const NAMES = ["byteLength", "maxByteLength", "growable"];

describe("SharedArrayBuffer accessors through the prototype chain", () => {
  test("a null prototype or a prototype without the accessors gives undefined", () => {
    for (const prototype of [null, {}]) {
      const buffer = Object.setPrototypeOf(new SharedArrayBuffer(4, { maxByteLength: 8 }), prototype);
      for (const name of NAMES) {
        expect(buffer[name]).toBeUndefined();
        expect(Reflect.get(buffer, name)).toBeUndefined();
      }
    }
  });

  test("a prototype inheriting from SharedArrayBuffer.prototype gives the built-in values", () => {
    const growable = Object.setPrototypeOf(new SharedArrayBuffer(4, { maxByteLength: 8 }),
      Object.create(SharedArrayBuffer.prototype));
    expect(growable.byteLength).toBe(4);
    expect(growable.maxByteLength).toBe(8);
    expect(growable.growable).toBe(true);

    const fixed = new SharedArrayBuffer(3);
    expect(fixed.maxByteLength).toBe(3);
    expect(fixed.growable).toBe(false);

    const empty = new SharedArrayBuffer(0, { maxByteLength: 0 });
    expect(empty.growable).toBe(true);
    expect(empty.maxByteLength).toBe(0);
    expect(empty.missing).toBeUndefined();
  });

  test("a data property, an own property or a getter found first is what is read", () => {
    const withData = Object.setPrototypeOf(new SharedArrayBuffer(4), { growable: "data" });
    expect(withData.growable).toBe("data");

    const withOwn = new SharedArrayBuffer(4);
    Object.defineProperty(withOwn, "byteLength", { value: "own" });
    expect(withOwn.byteLength).toBe("own");

    class Tagged extends SharedArrayBuffer {
      get maxByteLength() { return "subclass"; }
    }
    expect(new Tagged(3).maxByteLength).toBe("subclass");
  });

  test("the built-in getter is called with the receiver Reflect.get is given", () => {
    const buffer = new SharedArrayBuffer(4);

    expect(Reflect.get(buffer, "byteLength", new SharedArrayBuffer(16))).toBe(16);
    expect(() => Reflect.get(buffer, "byteLength", {})).toThrow(TypeError);
    expect(() => Reflect.get(buffer, "byteLength", new ArrayBuffer(2))).toThrow(TypeError);
  });

  test("the ArrayBuffer getters reject a SharedArrayBuffer that inherits them", () => {
    const buffer = Object.setPrototypeOf(new SharedArrayBuffer(4), ArrayBuffer.prototype);

    expect(() => buffer.byteLength).toThrow(TypeError);
    expect(() => buffer.detached).toThrow(TypeError);
    expect(buffer.growable).toBeUndefined();
  });
});
