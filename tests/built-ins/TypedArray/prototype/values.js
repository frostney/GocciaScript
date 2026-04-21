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

    test("iterator protocol", () => {
      const ta = new TA([1, 2]);
      const iter = ta.values();
      const first = iter.next();
      expect(first.done).toBe(false);
      expect(first.value).toBe(1);
      const second = iter.next();
      expect(second.done).toBe(false);
      expect(second.value).toBe(2);
      expect(iter.next().done).toBe(true);
    });
  });

  test.each([BigInt64Array, BigUint64Array])("%s values iterator", (TA) => {
    const ta = new TA([10n, 20n]);
    const iter = ta.values();
    expect(iter.next().value).toBe(10n);
    expect(iter.next().value).toBe(20n);
    expect(iter.next().done).toBe(true);
  });
});
