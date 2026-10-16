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

  test("throws when species returns a view of the source's own immutable buffer", () => {
    const buffer = new Uint8Array([1, 2, 3, 4, 0, 0, 0, 0]).buffer.transferToImmutable();
    const ta = new Uint8Array(buffer);
    ta.constructor = {
      [Symbol.species]: class {
        constructor(length) {
          return new Uint8Array(buffer, 4, length);
        }
      },
    };

    expect(() => ta.slice(0, 4)).toThrow(TypeError);
    expect(Array.from(new Uint8Array(buffer))).toEqual([1, 2, 3, 4, 0, 0, 0, 0]);
  });

  test("throws when species returns another element type over the source's own immutable buffer", () => {
    const buffer = new Uint8Array([1, 2, 3, 4, 0, 0, 0, 0]).buffer.transferToImmutable();
    const ta = new Uint8Array(buffer);
    ta.constructor = {
      [Symbol.species]: class {
        constructor(length) {
          return new Int8Array(buffer, 4, length);
        }
      },
    };

    expect(() => ta.slice(0, 4)).toThrow(TypeError);
    expect(Array.from(new Uint8Array(buffer))).toEqual([1, 2, 3, 4, 0, 0, 0, 0]);
  });

  test("throws for an empty slice when species returns an immutable view", () => {
    const buffer = new Uint8Array([1, 2]).buffer.transferToImmutable();
    const ta = new Uint8Array(buffer);
    ta.constructor = {
      [Symbol.species]: class {
        constructor(length) {
          return new Uint8Array(buffer, 0, length);
        }
      },
    };

    expect(() => ta.slice(0, 0)).toThrow(TypeError);
  });

  test("throws when species returns a view of a different immutable buffer", () => {
    const immutable = new Uint8Array([9, 9]).buffer.transferToImmutable();
    const ta = new Uint8Array([1, 2]);
    ta.constructor = {
      [Symbol.species]: class {
        constructor(length) {
          return new Uint8Array(immutable, 0, length);
        }
      },
    };

    expect(() => ta.slice()).toThrow(TypeError);
    expect(Array.from(new Uint8Array(immutable))).toEqual([9, 9]);
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
