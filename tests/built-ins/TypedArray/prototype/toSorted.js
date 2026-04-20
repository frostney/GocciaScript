describe("TypedArray.prototype.toSorted", () => {
  test("returns new sorted array", () => {
    const ta = new Int32Array([3, 1, 2]);
    const sorted = ta.toSorted();
    expect(sorted[0]).toBe(1);
    expect(sorted[1]).toBe(2);
    expect(sorted[2]).toBe(3);
    expect(ta[0]).toBe(3);
  });

  test("custom compare", () => {
    const ta = new Int32Array([1, 2, 3]);
    const sorted = ta.toSorted((a, b) => b - a);
    expect(sorted[0]).toBe(3);
    expect(sorted[1]).toBe(2);
    expect(sorted[2]).toBe(1);
  });

  test("on empty returns empty", () => {
    expect(new Int32Array(0).toSorted().length).toBe(0);
  });

  describe.each([Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float16Array, Float32Array, Float64Array])("%s", (TA) => {
    test("returns sorted copy", () => {
      const ta = new TA([3, 1, 2]);
      const sorted = ta.toSorted();
      expect(sorted[0]).toBe(1);
      expect(sorted[1]).toBe(2);
      expect(sorted[2]).toBe(3);
    });

    test("does not modify original", () => {
      const ta = new TA([3, 1, 2]);
      ta.toSorted();
      expect(ta[0]).toBe(3);
    });

    test("returns instance of same type", () => {
      const ta = new TA([3, 1, 2]);
      expect(ta.toSorted()).toBeInstanceOf(TA);
    });

    test("with comparator", () => {
      const ta = new TA([1, 2, 3]);
      const sorted = ta.toSorted((a, b) => b - a);
      expect(sorted[0]).toBe(3);
      expect(sorted[1]).toBe(2);
      expect(sorted[2]).toBe(1);
    });
  });

  test.each([BigInt64Array, BigUint64Array])("%s toSorted", (TA) => {
    const ta = new TA([3n, 1n, 2n]);
    const sorted = ta.toSorted();
    expect(sorted[0]).toBe(1n);
    expect(sorted[2]).toBe(3n);
    expect(ta[0]).toBe(3n);
  });
});
