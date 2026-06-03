describe("TypedArray.from", () => {
  test("is inherited from %TypedArray% with built-in method attributes", () => {
    const typedArrayIntrinsic = Object.getPrototypeOf(Uint8Array);

    expect(Object.getOwnPropertyDescriptor(Uint8Array, "from")).toBeUndefined();
    expect(Object.getOwnPropertyDescriptor(typedArrayIntrinsic, "from")).toEqual({
      value: Uint8Array.from,
      writable: true,
      enumerable: false,
      configurable: true,
    });
    expect(Int16Array.from([256])[0]).toBe(256);
    expect(Uint8Array.from.call(Int16Array, [256])[0]).toBe(256);
  });

  test("from another typed array with truncation", () => {
    const src = new Float64Array([1.7, 2.3, 3.9]);
    const ta = Int32Array.from(src);
    expect(ta.length).toBe(3);
    expect(ta[0]).toBe(1);
    expect(ta[1]).toBe(2);
    expect(ta[2]).toBe(3);
  });

  describe.each([Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float16Array, Float32Array, Float64Array])("%s", (TA) => {
    test(".from an array", () => {
      const ta = TA.from([1, 2, 3]);
      expect(ta.length).toBe(3);
      expect(ta[0]).toBe(1);
      expect(ta[1]).toBe(2);
      expect(ta[2]).toBe(3);
    });

    test(".from with map function", () => {
      const ta = TA.from([1, 2, 3], x => x * 2);
      expect(ta[0]).toBe(2);
      expect(ta[1]).toBe(4);
      expect(ta[2]).toBe(6);
    });

    test(".from empty array", () => {
      const ta = TA.from([]);
      expect(ta.length).toBe(0);
    });

    test("map function receives index", () => {
      const ta = TA.from([10, 20, 30], (val, idx) => idx);
      expect(ta[0]).toBe(0);
      expect(ta[1]).toBe(1);
      expect(ta[2]).toBe(2);
    });

    test("without arguments throws TypeError", () => {
      expect(() => TA.from()).toThrow(TypeError);
    });
  });

  describe("from iterable", () => {
    test("from custom iterable", () => {
      const iterable = { [Symbol.iterator]() { let i = 0; const vals = [1, 2, 3]; return { next() { return i < vals.length ? { value: vals[i++], done: false } : { done: true }; } }; } };
      const ta = Int32Array.from(iterable);
      expect(ta.length).toBe(3);
      expect(ta[0]).toBe(1);
      expect(ta[1]).toBe(2);
      expect(ta[2]).toBe(3);
    });

    test("from Set", () => {
      const ta = Uint8Array.from(new Set([4, 5, 6]));
      expect(ta.length).toBe(3);
      expect(ta[0]).toBe(4);
      expect(ta[1]).toBe(5);
      expect(ta[2]).toBe(6);
    });

    test("from iterable with mapFn", () => {
      const iterable = { [Symbol.iterator]() { let i = 0; const vals = [1, 2, 3]; return { next() { return i < vals.length ? { value: vals[i++], done: false } : { done: true }; } }; } };
      const ta = Int32Array.from(iterable, x => x * 10);
      expect(ta[0]).toBe(10);
      expect(ta[1]).toBe(20);
      expect(ta[2]).toBe(30);
    });

    test("from iterable mapFn receives index", () => {
      const iterable = { [Symbol.iterator]() { let i = 0; return { next() { if (i < 2) { i++; return { value: "x", done: false }; } return { done: true }; } }; } };
      const ta = Int32Array.from(iterable, (_, i) => i);
      expect(ta[0]).toBe(0);
      expect(ta[1]).toBe(1);
    });

    test("from empty iterable", () => {
      const iterable = { [Symbol.iterator]() { return { next() { return { done: true }; } }; } };
      const ta = Float64Array.from(iterable);
      expect(ta.length).toBe(0);
    });

    test("from Map values", () => {
      const m = new Map([["a", 5], ["b", 10]]);
      const ta = Int32Array.from(m.values());
      expect(ta.length).toBe(2);
      expect(ta[0]).toBe(5);
      expect(ta[1]).toBe(10);
    });
  });

  describe("from array-like", () => {
    test("from plain array-like object", () => {
      const ta = Int32Array.from({ 0: 10, 1: 20, 2: 30, length: 3 });
      expect(ta.length).toBe(3);
      expect(ta[0]).toBe(10);
      expect(ta[1]).toBe(20);
      expect(ta[2]).toBe(30);
    });

    test("from array-like with mapFn", () => {
      const ta = Int32Array.from({ 0: 1, 1: 2, length: 2 }, x => x * 100);
      expect(ta[0]).toBe(100);
      expect(ta[1]).toBe(200);
    });

    test("from array-like mapFn receives index", () => {
      const ta = Int32Array.from({ 0: "a", 1: "b", length: 2 }, (_, i) => i);
      expect(ta[0]).toBe(0);
      expect(ta[1]).toBe(1);
    });

    test("from empty array-like", () => {
      const ta = Int32Array.from({ length: 0 });
      expect(ta.length).toBe(0);
    });

    test("missing indices produce 0 or NaN", () => {
      const ta = Float64Array.from({ length: 2 });
      expect(ta.length).toBe(2);
      expect(isNaN(ta[0])).toBe(true);
      expect(isNaN(ta[1])).toBe(true);
    });
  });

  describe.each([BigInt64Array, BigUint64Array])("%s", (TA) => {
    test(".from array of BigInts", () => {
      const ta = TA.from([1n, 2n, 3n]);
      expect(ta.length).toBe(3);
      expect(ta[0]).toBe(1n);
      expect(ta[2]).toBe(3n);
    });

    test(".from with mapFn", () => {
      const ta = TA.from([1n, 2n, 3n], x => x * 10n);
      expect(ta[0]).toBe(10n);
      expect(ta[1]).toBe(20n);
      expect(ta[2]).toBe(30n);
    });

    test(".from rejects non-BigInt values", () => {
      expect(() => TA.from([1, 2, 3])).toThrow(TypeError);
    });

    test(".from rejects non-BigInt mapFn result", () => {
      expect(() => TA.from([1n], () => 1)).toThrow(TypeError);
    });
  });
});
