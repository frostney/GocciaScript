describe("TypedArray.from", () => {
  test("from another typed array", () => {
    const src = new Float64Array([1.7, 2.3, 3.9]);
    const ta = Int32Array.from(src);
    expect(ta.length).toBe(3);
    expect(ta[0]).toBe(1);
    expect(ta[1]).toBe(2);
    expect(ta[2]).toBe(3);
  });

  test("map function receives index", () => {
    const ta = Int32Array.from([10, 20, 30], (val, idx) => idx);
    expect(ta[0]).toBe(0);
    expect(ta[1]).toBe(1);
    expect(ta[2]).toBe(2);
  });

  test("without arguments throws TypeError", () => {
    expect(() => Int32Array.from()).toThrow(TypeError);
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
