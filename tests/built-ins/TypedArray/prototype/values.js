describe("TypedArray.prototype.values", () => {
  describe.each([Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float16Array, Float32Array, Float64Array])("%s", (TA) => {
    test("iterates over values", () => {
      const ta = new TA([1, 2, 3]);
      const vals = [...ta.values()];
      expect(vals).toEqual([1, 2, 3]);
    });

    test("empty array yields nothing", () => {
      const ta = new TA(0);
      const vals = [...ta.values()];
      expect(vals).toEqual([]);
    });
  });

  test.each([BigInt64Array, BigUint64Array])("%s values iterator", (TA) => {
    const ta = new TA([10n, 20n]);
    const vals = [...ta.values()];
    expect(vals[0]).toBe(10n);
    expect(vals[1]).toBe(20n);
  });
});
