/*---
description: >
  BYTES_PER_ELEMENT is a data property of each typed array constructor's
  prototype, so an instance reads it through its prototype chain
features: [TypedArray]
---*/

const TypedArrayPrototype = Object.getPrototypeOf(Uint8Array.prototype);

describe("TypedArray.prototype.BYTES_PER_ELEMENT", () => {
  test("is a constant data property of each constructor's prototype", () => {
    const sizes = [
      [Int8Array, 1],
      [Uint8Array, 1],
      [Uint8ClampedArray, 1],
      [Int16Array, 2],
      [Uint16Array, 2],
      [Int32Array, 4],
      [Uint32Array, 4],
      [Float16Array, 2],
      [Float32Array, 4],
      [Float64Array, 8],
      [BigInt64Array, 8],
      [BigUint64Array, 8],
    ];

    for (const [TA, size] of sizes) {
      expect(Object.getOwnPropertyDescriptor(TA.prototype, "BYTES_PER_ELEMENT")).toEqual({
        value: size,
        writable: false,
        enumerable: false,
        configurable: false,
      });
      expect(new TA(2).BYTES_PER_ELEMENT).toBe(size);
    }
  });

  test("is not a property of %TypedArray%.prototype or of an instance", () => {
    const ta = new Float64Array(2);

    expect(Object.hasOwn(TypedArrayPrototype, "BYTES_PER_ELEMENT")).toBe(false);
    expect(TypedArrayPrototype.BYTES_PER_ELEMENT).toBeUndefined();
    expect(Object.hasOwn(ta, "BYTES_PER_ELEMENT")).toBe(false);
    expect(Object.getOwnPropertyDescriptor(ta, "BYTES_PER_ELEMENT")).toBeUndefined();
    expect("BYTES_PER_ELEMENT" in ta).toBe(true);
  });

  test("a replaced prototype answers the read", () => {
    const ta = Object.setPrototypeOf(new Uint16Array(2), { BYTES_PER_ELEMENT: "e" });

    expect(ta.BYTES_PER_ELEMENT).toBe("e");
    expect(Reflect.get(ta, "BYTES_PER_ELEMENT")).toBe("e");
  });

  test("the prototype of another constructor answers with its own element size", () => {
    const ta = Object.setPrototypeOf(new Uint16Array(2), Float64Array.prototype);

    expect(ta.BYTES_PER_ELEMENT).toBe(8);
    expect(ta.byteLength).toBe(4);
  });

  test("a prototype chain without a constructor's prototype has no BYTES_PER_ELEMENT", () => {
    const withoutPrototype = Object.setPrototypeOf(new Uint16Array(2), null);
    const onIntrinsic = Object.setPrototypeOf(new Uint16Array(2), TypedArrayPrototype);

    expect(withoutPrototype.BYTES_PER_ELEMENT).toBeUndefined();
    expect("BYTES_PER_ELEMENT" in withoutPrototype).toBe(false);
    expect(onIntrinsic.BYTES_PER_ELEMENT).toBeUndefined();
    expect(onIntrinsic.length).toBe(2);
  });

  test("a subclass can override it and inherits it otherwise", () => {
    class Overriding extends Uint16Array {
      get BYTES_PER_ELEMENT() {
        return 96;
      }
    }
    class Inheriting extends Uint16Array {}

    expect(new Overriding(2).BYTES_PER_ELEMENT).toBe(96);
    expect(new Inheriting(2).BYTES_PER_ELEMENT).toBe(2);
    expect(Inheriting.BYTES_PER_ELEMENT).toBe(2);
  });

  test("an own property shadows it until it is deleted", () => {
    const ta = new Uint16Array(2);

    Object.defineProperty(ta, "BYTES_PER_ELEMENT", { value: "own", configurable: true });
    expect(ta.BYTES_PER_ELEMENT).toBe("own");
    expect(Object.hasOwn(ta, "BYTES_PER_ELEMENT")).toBe(true);

    expect(delete ta.BYTES_PER_ELEMENT).toBe(true);
    expect(ta.BYTES_PER_ELEMENT).toBe(2);
  });

  test("typed arrays created by the runtime inherit it from their constructor's prototype", () => {
    const floats = new Float32Array([3, 1, 2]);

    expect(new TextEncoder().encode("ab").BYTES_PER_ELEMENT).toBe(1);
    expect(floats.map((value) => value).BYTES_PER_ELEMENT).toBe(4);
    expect(floats.slice(1).BYTES_PER_ELEMENT).toBe(4);
    expect(floats.subarray(1).BYTES_PER_ELEMENT).toBe(4);
    expect(floats.filter((value) => value > 1).BYTES_PER_ELEMENT).toBe(4);
    expect(floats.toSorted().BYTES_PER_ELEMENT).toBe(4);
    expect(floats.toReversed().BYTES_PER_ELEMENT).toBe(4);
    expect(floats.with(0, 9).BYTES_PER_ELEMENT).toBe(4);
    expect(Float32Array.from([1, 2]).BYTES_PER_ELEMENT).toBe(4);
    expect(Float32Array.of(1).BYTES_PER_ELEMENT).toBe(4);
    expect(new Int16Array(floats.buffer, 2, 2).BYTES_PER_ELEMENT).toBe(2);
  });
});
