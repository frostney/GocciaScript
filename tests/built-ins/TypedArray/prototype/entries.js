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

    test("iterator protocol", () => {
      const ta = new TA([1, 2]);
      const iter = ta.entries();
      const first = iter.next();
      expect(first.done).toBe(false);
      expect(first.value).toEqual([0, 1]);
      const second = iter.next();
      expect(second.done).toBe(false);
      expect(second.value).toEqual([1, 2]);
      const third = iter.next();
      expect(third.done).toBe(true);
      expect(third.value).toBeUndefined();
    });
  });

  test.each([BigInt64Array, BigUint64Array])("%s entries iterator", (TA) => {
    const ta = new TA([10n, 20n]);
    const iter = ta.entries();
    const first = iter.next();
    expect(first.done).toBe(false);
    expect(first.value[0]).toBe(0);
    expect(first.value[1]).toBe(10n);
    const second = iter.next();
    expect(second.done).toBe(false);
    expect(second.value[0]).toBe(1);
    expect(second.value[1]).toBe(20n);
    expect(iter.next().done).toBe(true);
  });
});
