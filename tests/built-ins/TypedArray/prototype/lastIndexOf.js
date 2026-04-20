describe("TypedArray.prototype.lastIndexOf", () => {
  test("finds last occurrence", () => {
    const ta = new Int32Array([10, 20, 30, 20]);
    expect(ta.lastIndexOf(20)).toBe(3);
  });

  test("returns -1 when not found", () => {
    const ta = new Int32Array([10, 20, 30]);
    expect(ta.lastIndexOf(99)).toBe(-1);
  });

  test("fromIndex parameter", () => {
    const ta = new Int32Array([1, 2, 3, 2, 1]);
    expect(ta.lastIndexOf(2, 2)).toBe(1);
  });

  test("negative fromIndex", () => {
    const ta = new Int32Array([1, 2, 3, 2, 1]);
    expect(ta.lastIndexOf(2, -2)).toBe(3);
  });

  describe.each([Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float16Array, Float32Array, Float64Array])("%s", (TA) => {
    test("finds last occurrence", () => {
      const ta = new TA([1, 2, 3, 2]);
      expect(ta.lastIndexOf(2)).toBe(3);
    });

    test("returns -1 when not found", () => {
      const ta = new TA([1, 2, 3]);
      expect(ta.lastIndexOf(99)).toBe(-1);
    });

    test("fromIndex parameter", () => {
      const ta = new TA([1, 2, 3, 2]);
      expect(ta.lastIndexOf(2, 2)).toBe(1);
    });
  });

  test.each([BigInt64Array, BigUint64Array])("%s lastIndexOf", (TA) => {
    const ta = new TA([1n, 2n, 3n, 2n]);
    expect(ta.lastIndexOf(2n)).toBe(3);
  });
});
