describe("TypedArray.prototype.slice", () => {
  test("returns independent copy (Float32Array)", () => {
    const ta = new Float32Array([1.5, 2.5]);
    const sliced = ta.slice();
    ta[0] = 99;
    expect(sliced[0]).toBe(1.5);
  });

  describe.each([Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float16Array, Float32Array, Float64Array])("%s", (TA) => {
    test("slice entire array", () => {
      const ta = new TA([1, 2, 3]);
      const sliced = ta.slice();
      expect(sliced.length).toBe(3);
      expect(sliced[0]).toBe(1);
      expect(sliced[2]).toBe(3);
    });

    test("slice with start", () => {
      const ta = new TA([1, 2, 3, 4]);
      const sliced = ta.slice(1);
      expect(sliced.length).toBe(3);
      expect(sliced[0]).toBe(2);
    });

    test("slice with start and end", () => {
      const ta = new TA([1, 2, 3, 4]);
      const sliced = ta.slice(1, 3);
      expect(sliced.length).toBe(2);
      expect(sliced[0]).toBe(2);
      expect(sliced[1]).toBe(3);
    });

    test("returns instance of same type", () => {
      const ta = new TA([1, 2, 3]);
      const sliced = ta.slice();
      expect(sliced).toBeInstanceOf(TA);
    });

    test("negative indices", () => {
      const ta = new TA([1, 2, 3, 4]);
      const sliced = ta.slice(-2);
      expect(sliced.length).toBe(2);
      expect(sliced[0]).toBe(3);
      expect(sliced[1]).toBe(4);
    });

    test("on empty returns empty", () => {
      const ta = new TA(0);
      const sliced = ta.slice();
      expect(sliced.length).toBe(0);
    });

    test("start > end returns empty", () => {
      const ta = new TA([1, 2, 3]);
      const sliced = ta.slice(2, 1);
      expect(sliced.length).toBe(0);
    });

    test("start beyond length returns empty", () => {
      const ta = new TA([1, 2, 3]);
      const sliced = ta.slice(10);
      expect(sliced.length).toBe(0);
    });

    test("both negative", () => {
      const ta = new TA([1, 2, 3, 4, 5]);
      const sliced = ta.slice(-3, -1);
      expect(sliced.length).toBe(2);
      expect(sliced[0]).toBe(3);
      expect(sliced[1]).toBe(4);
    });
  });

  test.each([BigInt64Array, BigUint64Array])("%s slice", (TA) => {
    const ta = new TA([1n, 2n, 3n, 4n]);
    const sliced = ta.slice(1, 3);
    expect(sliced.length).toBe(2);
    expect(sliced[0]).toBe(2n);
    expect(sliced[1]).toBe(3n);
  });

  test("uses constructor Symbol.species override", () => {
    const ta = new Uint8Array([1, 2, 3]);
    ta.constructor = { [Symbol.species]: Float32Array };

    const sliced = ta.slice(0, 2);

    expect(Object.prototype.toString.call(sliced)).toBe("[object Float32Array]");
    expect(sliced.length).toBe(2);
    expect(sliced.byteLength).toBe(8);
    expect(sliced[0]).toBe(1);
    expect(sliced[1]).toBe(2);
  });

  test("throws when species changes BigInt content type", () => {
    const ta = new BigInt64Array([1n]);
    ta.constructor = { [Symbol.species]: Float64Array };

    expect(() => ta.slice()).toThrow(TypeError);
  });

  test("converts when species returns another element type", () => {
    const ta = new Uint8Array([1, 200, 255]);
    ta.constructor = { [Symbol.species]: Int8Array };
    const result = ta.slice();
    expect(result instanceof Int8Array).toBe(true);
    expect(Array.from(result)).toEqual([1, -56, -1]);
  });

  test("copies bytes in ascending order into a species view of the same buffer", () => {
    const ta = new Uint8Array([1, 2, 3, 4, 5, 6, 7, 8]);
    ta.constructor = {
      [Symbol.species]: class {
        constructor(length) {
          return new Uint8Array(ta.buffer, 1, length);
        }
      }
    };
    ta.slice(0, 4);
    expect(Array.from(ta)).toEqual([1, 1, 1, 1, 1, 6, 7, 8]);
  });

  test("copies into a species view below the source on the same buffer", () => {
    const ta = new Uint8Array([1, 2, 3, 4, 5, 6, 7, 8]);
    ta.constructor = {
      [Symbol.species]: class {
        constructor(length) {
          return new Uint8Array(ta.buffer, 0, length);
        }
      }
    };
    ta.slice(2, 6);
    expect(Array.from(ta)).toEqual([3, 4, 5, 6, 5, 6, 7, 8]);
  });

  test("keeps a non-canonical NaN bit pattern", () => {
    const bytes = [1, 0, 0, 0, 0, 0, 0xf8, 0x7f];
    const result = new Float64Array(new Uint8Array(bytes).buffer).slice();
    expect(Array.from(new Uint8Array(result.buffer))).toEqual(bytes);
  });

  test("copies only what remains when start coercion shrinks a resizable buffer", () => {
    const rab = new ArrayBuffer(8, { maxByteLength: 16 });
    const ta = new Uint8Array(rab);
    ta.set([1, 2, 3, 4, 5, 6, 7, 8]);
    const start = {
      valueOf() {
        rab.resize(4);
        return 1;
      }
    };
    expect(Array.from(ta.slice(start, 6))).toEqual([2, 3, 4, 0, 0]);
  });

  test("copies a SharedArrayBuffer view", () => {
    const ta = new Uint8Array(new SharedArrayBuffer(4));
    ta.set([1, 2, 3, 4]);
    expect(Array.from(ta.slice(1, 3))).toEqual([2, 3]);
  });

  test("throws on detached buffer", () => {
    const ta = new Int32Array([1, 2, 3]);
    ta.buffer.transfer();
    expect(() => ta.slice()).toThrow(TypeError);
  });

  test("throws if start coercion detaches buffer before copy", () => {
    const ta = new Int32Array([1, 2, 3, 4]);
    const start = {
      valueOf() {
        ta.buffer.transfer();
        return 1;
      }
    };

    expect(() => ta.slice(start)).toThrow(TypeError);
  });

  test("throws if end coercion detaches buffer before copy", () => {
    const ta = new Int32Array([1, 2, 3, 4]);
    const end = {
      valueOf() {
        ta.buffer.transfer();
        return 3;
      }
    };

    expect(() => ta.slice(1, end)).toThrow(TypeError);
  });

  test("returns empty if detachment happens after the slice is clamped empty", () => {
    const ta = new Int32Array([1, 2, 3]);
    const start = {
      valueOf() {
        ta.buffer.transfer();
        return 3;
      }
    };

    expect(ta.slice(start).length).toBe(0);
  });
});

describe("TypedArray.prototype.slice non-finite range arguments", () => {
  test("Infinity start yields an empty copy", () => {
    expect(new Int8Array([1, 2]).slice(Infinity).length).toBe(0);
  });

  test("-Infinity start copies the whole array", () => {
    expect(Array.from(new Int8Array([1, 2]).slice(-Infinity))).toEqual([1, 2]);
  });
});
