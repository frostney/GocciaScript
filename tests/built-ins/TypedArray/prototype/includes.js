describe("TypedArray.prototype.includes", () => {
  test("NaN handling with Float64Array", () => {
    const ta = new Float64Array([1, NaN, 3]);
    expect(ta.includes(NaN)).toBe(true);
  });

  test("0 vs -0 with Float64Array", () => {
    const ta = new Float64Array([0]);
    expect(ta.includes(-0)).toBe(true);
  });

  describe.each([Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float16Array, Float32Array, Float64Array])("%s", (TA) => {
    test("returns true when element exists", () => {
      const ta = new TA([1, 2, 3]);
      expect(ta.includes(2)).toBe(true);
    });

    test("returns false when not found", () => {
      const ta = new TA([1, 2, 3]);
      expect(ta.includes(99)).toBe(false);
    });

    test("with fromIndex", () => {
      const ta = new TA([1, 2, 3]);
      expect(ta.includes(1, 1)).toBe(false);
    });

    test("on empty returns false", () => {
      const ta = new TA(0);
      expect(ta.includes(1)).toBe(false);
    });

    test("with negative fromIndex", () => {
      const ta = new TA([1, 2, 3]);
      expect(ta.includes(1, -2)).toBe(false);
      expect(ta.includes(2, -2)).toBe(true);
    });
  });

  test.each([BigInt64Array, BigUint64Array])("%s includes", (TA) => {
    const ta = new TA([1n, 2n, 3n]);
    expect(ta.includes(2n)).toBe(true);
    expect(ta.includes(4n)).toBe(false);
  });
});
