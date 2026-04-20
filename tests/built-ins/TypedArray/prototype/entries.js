describe("TypedArray.prototype.entries", () => {
  describe.each([Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float16Array, Float32Array, Float64Array])("%s", (TA) => {
    test("iterates over [index, value] pairs", () => {
      const ta = new TA([1, 2, 3]);
      const entries = [...ta.entries()];
      expect(entries).toEqual([[0, 1], [1, 2], [2, 3]]);
    });

    test("empty array yields nothing", () => {
      const ta = new TA(0);
      const entries = [...ta.entries()];
      expect(entries).toEqual([]);
    });
  });

  test.each([BigInt64Array, BigUint64Array])("%s entries iterator", (TA) => {
    const ta = new TA([10n, 20n]);
    const entries = [...ta.entries()];
    expect(entries[0][0]).toBe(0);
    expect(entries[0][1]).toBe(10n);
    expect(entries[1][0]).toBe(1);
    expect(entries[1][1]).toBe(20n);
  });
});
