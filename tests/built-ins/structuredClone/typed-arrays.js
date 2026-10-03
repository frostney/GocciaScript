/*---
description: structuredClone clones typed arrays and DataView with their buffer, offset and length
features: [structuredClone, TypedArray, DataView, ArrayBuffer]
---*/

// StructuredSerializeInternal serializes an ArrayBuffer view as its buffer
// (through the memory map), its kind, byte offset and length, so a clone keeps
// its kind and views over one buffer stay over one cloned buffer.
describe("typed array cloning", () => {
  test("keeps the kind, length and elements", () => {
    const original = new Float32Array([3, 1, 2]);
    const clone = structuredClone(original);
    expect(Object.prototype.toString.call(clone)).toBe("[object Float32Array]");
    expect(Object.getPrototypeOf(clone)).toBe(Float32Array.prototype);
    expect(clone.length).toBe(3);
    expect([...clone]).toEqual([3, 1, 2]);
    expect(clone).not.toBe(original);
    expect(clone.buffer).not.toBe(original.buffer);
  });

  test("clones every kind", () => {
    const kinds = [
      Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array,
      Int32Array, Uint32Array, Float32Array, Float64Array,
    ];
    for (const Kind of kinds) {
      const clone = structuredClone(new Kind([1, 2]));
      expect(clone instanceof Kind).toBe(true);
      expect([...clone]).toEqual([1, 2]);
    }
    const big = structuredClone(new BigInt64Array([1n, -2n]));
    expect(big instanceof BigInt64Array).toBe(true);
    expect([...big]).toEqual([1n, -2n]);
    expect(structuredClone(new BigUint64Array([3n]))[0]).toBe(3n);
  });

  test("copies the elements rather than sharing them", () => {
    const original = new Uint8Array([1, 2]);
    const clone = structuredClone(original);
    original[0] = 9;
    expect(clone[0]).toBe(1);
  });

  test("keeps the byte offset and the whole buffer", () => {
    const buffer = new ArrayBuffer(8);
    new Uint8Array(buffer).set([0, 1, 2, 3, 4, 5, 6, 7]);
    const clone = structuredClone(new Uint8Array(buffer, 2, 3));
    expect(clone.byteOffset).toBe(2);
    expect(clone.length).toBe(3);
    expect([...clone]).toEqual([2, 3, 4]);
    expect(clone.buffer.byteLength).toBe(8);
  });

  test("views over one buffer share one cloned buffer", () => {
    const buffer = new ArrayBuffer(8);
    const [first, second] = structuredClone([
      new Uint8Array(buffer),
      new Uint16Array(buffer, 4),
    ]);
    expect(first.buffer).toBe(second.buffer);
    expect(second.byteOffset).toBe(4);
    expect(second.length).toBe(2);
    first[4] = 1;
    expect(second[0]).toBe(1);
  });

  test("a view and its buffer cloned together stay connected", () => {
    const buffer = new ArrayBuffer(4);
    const clone = structuredClone({ buffer, view: new Uint8Array(buffer) });
    expect(clone.view.buffer).toBe(clone.buffer);
  });

  test("one view cloned twice is one clone", () => {
    const view = new Uint8Array(2);
    const [first, second] = structuredClone([view, view]);
    expect(first).toBe(second);
  });

  test("drops named own properties and the source prototype", () => {
    class Bytes extends Uint8Array {}
    const original = new Bytes([1]);
    original.extra = 1;
    const clone = structuredClone(original);
    expect(clone.extra).toBeUndefined();
    expect(Object.getPrototypeOf(clone)).toBe(Uint8Array.prototype);
  });

  test("a length-tracking view keeps tracking its cloned buffer", () => {
    const buffer = new ArrayBuffer(4, { maxByteLength: 16 });
    const clone = structuredClone(new Uint8Array(buffer));
    expect(clone.buffer.resizable).toBe(true);
    expect(clone.buffer.maxByteLength).toBe(16);
    clone.buffer.resize(8);
    expect(clone.length).toBe(8);
  });

  test("a fixed-length view over a resizable buffer keeps its length", () => {
    const buffer = new ArrayBuffer(4, { maxByteLength: 16 });
    const clone = structuredClone(new Uint8Array(buffer, 0, 2));
    clone.buffer.resize(8);
    expect(clone.length).toBe(2);
  });

  test("throws DataCloneError for a view over a detached buffer", () => {
    const buffer = new ArrayBuffer(4);
    const view = new Uint8Array(buffer);
    buffer.transfer();
    expect(() => structuredClone(view)).toThrow(DOMException);
    try {
      structuredClone(view);
    } catch (e) {
      expect(e.name).toBe("DataCloneError");
    }
  });

  test("throws DataCloneError for an out-of-bounds view", () => {
    const buffer = new ArrayBuffer(4, { maxByteLength: 8 });
    const view = new Uint8Array(buffer, 2, 2);
    buffer.resize(1);
    expect(() => structuredClone(view)).toThrow(DOMException);
  });
});

describe("DataView cloning", () => {
  test("keeps the offset, length and bytes", () => {
    const buffer = new ArrayBuffer(8);
    const view = new DataView(buffer, 1, 4);
    view.setUint8(1, 7);
    const clone = structuredClone(view);
    expect(Object.prototype.toString.call(clone)).toBe("[object DataView]");
    expect(Object.getPrototypeOf(clone)).toBe(DataView.prototype);
    expect(clone.byteOffset).toBe(1);
    expect(clone.byteLength).toBe(4);
    expect(clone.getUint8(1)).toBe(7);
    expect(clone.buffer.byteLength).toBe(8);
    expect(clone.buffer).not.toBe(buffer);
  });

  test("shares one cloned buffer with a typed array over the same buffer", () => {
    const buffer = new ArrayBuffer(4);
    const clone = structuredClone([new DataView(buffer), new Uint8Array(buffer)]);
    expect(clone[0].buffer).toBe(clone[1].buffer);
  });

  test("a length-tracking DataView keeps tracking its cloned buffer", () => {
    const buffer = new ArrayBuffer(2, { maxByteLength: 8 });
    const clone = structuredClone(new DataView(buffer));
    clone.buffer.resize(6);
    expect(clone.byteLength).toBe(6);
  });

  test("throws DataCloneError for a DataView over a detached buffer", () => {
    const buffer = new ArrayBuffer(4);
    const view = new DataView(buffer);
    buffer.transfer();
    expect(() => structuredClone(view)).toThrow(DOMException);
  });
});

describe("ArrayBuffer details the views depend on", () => {
  test("a resizable buffer stays resizable", () => {
    const clone = structuredClone(new ArrayBuffer(4, { maxByteLength: 8 }));
    expect(clone.resizable).toBe(true);
    expect(clone.maxByteLength).toBe(8);
  });

  test("throws DataCloneError for a detached buffer", () => {
    const buffer = new ArrayBuffer(4);
    buffer.transfer();
    expect(() => structuredClone(buffer)).toThrow(DOMException);
  });
});
