describe("TypedArray.prototype.set", () => {
  test("cross-type set truncates", () => {
    const src = new Float64Array([1.5, 2.5, 3.5]);
    const dst = new Int32Array(3);
    dst.set(src);
    expect(dst[0]).toBe(1);
    expect(dst[1]).toBe(2);
    expect(dst[2]).toBe(3);
  });

  describe.each([Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float16Array, Float32Array, Float64Array])("%s", (TA) => {
    test("set from array", () => {
      const ta = new TA(3);
      ta.set([1, 2, 3]);
      expect(ta[0]).toBe(1);
      expect(ta[1]).toBe(2);
      expect(ta[2]).toBe(3);
    });

    test("set with offset", () => {
      const ta = new TA(5);
      ta.set([4, 5], 2);
      expect(ta[0]).toBe(0);
      expect(ta[2]).toBe(4);
      expect(ta[3]).toBe(5);
    });

    test("set from another typed array", () => {
      const src = new TA([7, 8, 9]);
      const dst = new TA(3);
      dst.set(src);
      expect(dst[0]).toBe(7);
      expect(dst[1]).toBe(8);
      expect(dst[2]).toBe(9);
    });

    test("negative offset throws RangeError", () => {
      const ta = new TA(4);
      expect(() => ta.set([1], -1)).toThrow(RangeError);
    });

    test("source too large throws RangeError", () => {
      const ta = new TA(2);
      expect(() => ta.set([1, 2, 3])).toThrow(RangeError);
    });

    test("offset causing overflow throws RangeError", () => {
      const ta = new TA(3);
      expect(() => ta.set([1, 2], 2)).toThrow(RangeError);
    });

    test("throws when target buffer is already detached", () => {
      const ta = new TA(3);
      ta.buffer.transfer();
      expect(() => ta.set([1])).toThrow(TypeError);
    });
  });

  describe("set from array-like", () => {
    test("set from plain array-like object", () => {
      const ta = new Int32Array(3);
      ta.set({ 0: 5, 1: 6, length: 2 });
      expect(ta[0]).toBe(5);
      expect(ta[1]).toBe(6);
      expect(ta[2]).toBe(0);
    });

    test("set from array-like with offset", () => {
      const ta = new Int32Array(4);
      ta.set({ 0: 10, 1: 20, length: 2 }, 2);
      expect(ta[0]).toBe(0);
      expect(ta[1]).toBe(0);
      expect(ta[2]).toBe(10);
      expect(ta[3]).toBe(20);
    });

    test("set from empty array-like", () => {
      const ta = new Int32Array([1, 2, 3]);
      ta.set({ length: 0 });
      expect(ta[0]).toBe(1);
      expect(ta[1]).toBe(2);
      expect(ta[2]).toBe(3);
    });

    test("array-like too large throws RangeError", () => {
      const ta = new Int32Array(2);
      expect(() => ta.set({ 0: 1, 1: 2, 2: 3, length: 3 })).toThrow(RangeError);
    });

    test("array-like with offset overflow throws RangeError", () => {
      const ta = new Int32Array(3);
      expect(() => ta.set({ 0: 1, 1: 2, length: 2 }, 2)).toThrow(RangeError);
    });

    test("array-like length getter can detach target buffer", () => {
      const ta = new Int32Array(4);
      const buffer = ta.buffer;
      let sourceRead = false;
      ta.set({
        get 0() {
          sourceRead = true;
          return 17;
        },
        get length() {
          buffer.transfer();
          return 1;
        }
      }, 1);

      expect(buffer.detached).toBe(true);
      expect(sourceRead).toBe(true);
      expect(ta.length).toBe(0);
      expect(ta.byteLength).toBe(0);
      expect(ta.byteOffset).toBe(0);
    });
  });

  test.each([BigInt64Array, BigUint64Array])("%s set from BigInt typed array", (TA) => {
    const src = new TA([10n, 20n]);
    const dst = new TA(4);
    dst.set(src, 1);
    expect(dst[0]).toBe(0n);
    expect(dst[1]).toBe(10n);
    expect(dst[2]).toBe(20n);
  });

  test("throws when typed array source buffer is already detached", () => {
    const src = new Int32Array([1]);
    const dst = new Int32Array(1);
    src.buffer.transfer();
    expect(() => dst.set(src)).toThrow(TypeError);
  });
});

describe("TypedArray.prototype.set from a typed array of the same element type", () => {
  test("copies a source that overlaps the target from below", () => {
    const ta = new Uint8Array([1, 2, 3, 4, 5, 6, 7, 8]);
    ta.set(ta.subarray(0, 6), 2);
    expect(Array.from(ta)).toEqual([1, 2, 1, 2, 3, 4, 5, 6]);
  });

  test("copies a source that overlaps the target from above", () => {
    const ta = new Uint8Array([1, 2, 3, 4, 5, 6, 7, 8]);
    ta.set(ta.subarray(2), 0);
    expect(Array.from(ta)).toEqual([3, 4, 5, 6, 7, 8, 7, 8]);
  });

  test.each([Int16Array, Float32Array, Float64Array])("%s copies overlapping multi-byte elements", (TA) => {
    const ta = new TA([1, 2, 3, 4, 5]);
    ta.set(ta.subarray(0, 4), 1);
    expect(Array.from(ta)).toEqual([1, 1, 2, 3, 4]);
  });

  test.each([BigInt64Array, BigUint64Array])("%s copies overlapping elements", (TA) => {
    const ta = new TA([1n, 2n, 3n, 4n]);
    ta.set(ta.subarray(1), 0);
    expect(Array.from(ta)).toEqual([2n, 3n, 4n, 4n]);
  });

  test("keeps the bit pattern of a non-canonical NaN", () => {
    const bytes = [1, 0, 0, 0, 0, 0, 0xf8, 0x7f];
    const src = new Float64Array(new Uint8Array(bytes).buffer);
    const dst = new Float64Array(1);
    dst.set(src);
    expect(Array.from(new Uint8Array(dst.buffer))).toEqual(bytes);
  });

  test("copies Float16Array bits", () => {
    const src = new Float16Array([1.5, -0, Infinity, 65504]);
    const dst = new Float16Array(4);
    dst.set(src);
    expect(Array.from(new Uint16Array(dst.buffer))).toEqual(Array.from(new Uint16Array(src.buffer)));
    expect(Object.is(dst[1], -0)).toBe(true);
  });

  test("copies Uint8ClampedArray bytes unchanged", () => {
    const src = new Uint8ClampedArray([0, 128, 255]);
    const dst = new Uint8ClampedArray(4);
    dst.set(src, 1);
    expect(Array.from(dst)).toEqual([0, 0, 128, 255]);
  });

  test("still clamps a source of another element type", () => {
    const dst = new Uint8ClampedArray(3);
    dst.set(new Int8Array([-1, 127, -128]));
    expect(Array.from(dst)).toEqual([0, 127, 0]);
  });

  test("reads a source of another type on the same buffer before writing", () => {
    const buffer = new ArrayBuffer(8);
    const bytes = new Uint8Array(buffer);
    bytes.set([1, 2, 3, 4, 5, 6, 7, 8]);
    const words = new Uint16Array(buffer, 0, 2);
    bytes.set(words, 2);
    expect(Array.from(bytes)).toEqual([1, 2, 1, 3, 5, 6, 7, 8]);
  });

  test("throws when offset coercion detaches the target", () => {
    const src = new Uint8Array([1, 2]);
    const dst = new Uint8Array(4);
    const offset = {
      valueOf() {
        dst.buffer.transfer();
        return 0;
      }
    };
    expect(() => dst.set(src, offset)).toThrow(TypeError);
  });

  test("throws when offset coercion detaches the source", () => {
    const src = new Uint8Array([1, 2]);
    const dst = new Uint8Array(4);
    const offset = {
      valueOf() {
        src.buffer.transfer();
        return 0;
      }
    };
    expect(() => dst.set(src, offset)).toThrow(TypeError);
  });

  test("reads a length-tracking source's length after offset coercion", () => {
    const rab = new ArrayBuffer(8, { maxByteLength: 16 });
    const src = new Uint8Array(rab);
    src.set([1, 2, 3, 4, 5, 6, 7, 8]);
    const dst = new Uint8Array(8);
    const offset = {
      valueOf() {
        rab.resize(4);
        return 1;
      }
    };
    dst.set(src, offset);
    expect(Array.from(dst)).toEqual([0, 1, 2, 3, 4, 0, 0, 0]);
  });

  test("copies into a length-tracking target on a resizable buffer", () => {
    const rab = new ArrayBuffer(4, { maxByteLength: 8 });
    const dst = new Uint8Array(rab);
    rab.resize(8);
    dst.set(new Uint8Array([9, 8, 7]), 5);
    expect(Array.from(dst)).toEqual([0, 0, 0, 0, 0, 9, 8, 7]);
  });

  test("throws when offset coercion shrinks the target out of bounds", () => {
    const rab = new ArrayBuffer(8, { maxByteLength: 16 });
    const dst = new Uint8Array(rab, 4, 4);
    const offset = {
      valueOf() {
        rab.resize(6);
        return 0;
      }
    };
    expect(() => dst.set(new Uint8Array(2), offset)).toThrow(TypeError);
  });

  test("throws a RangeError when the source no longer fits after offset coercion", () => {
    const rab = new ArrayBuffer(8, { maxByteLength: 16 });
    const dst = new Uint8Array(rab);
    const offset = {
      valueOf() {
        rab.resize(2);
        return 0;
      }
    };
    expect(() => dst.set(new Uint8Array(4), offset)).toThrow(RangeError);
  });

  test("copies between SharedArrayBuffer and ArrayBuffer views", () => {
    const shared = new Uint8Array(new SharedArrayBuffer(4));
    shared.set(new Uint8Array([4, 3, 2, 1]));
    const plain = new Uint8Array(4);
    plain.set(shared);
    expect(Array.from(plain)).toEqual([4, 3, 2, 1]);
  });

  test("copies an overlapping source on the same SharedArrayBuffer", () => {
    const shared = new Uint8Array(new SharedArrayBuffer(6));
    shared.set([1, 2, 3, 4, 5, 6]);
    shared.set(shared.subarray(0, 4), 2);
    expect(Array.from(shared)).toEqual([1, 2, 1, 2, 3, 4]);
  });

  test("copies large arrays", () => {
    const src = new Uint8Array(70000);
    for (const i of src.keys()) src[i] = i & 255;
    const dst = new Uint8Array(70001);
    dst.set(src, 1);
    expect(dst[0]).toBe(0);
    expect(dst[1]).toBe(0);
    expect(dst[256]).toBe(255);
    expect(dst[70000]).toBe(69999 & 255);
  });
});
