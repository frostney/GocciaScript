describe("TypedArray [Symbol.iterator]", () => {
  describe.each([Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float16Array, Float32Array, Float64Array])("%s", (TA) => {
    test("spread works", () => {
      const ta = new TA([1, 2, 3]);
      const arr = [...ta];
      expect(arr).toEqual([1, 2, 3]);
    });

    test("for-of iterates values", () => {
      const ta = new TA([1, 2, 3]);
      const collected = [];
      for (const v of ta) collected.push(v);
      expect(collected).toEqual([1, 2, 3]);
    });

    test("destructuring works", () => {
      const ta = new TA([10, 20, 30]);
      const [a, b, c] = ta;
      expect(a).toBe(10);
      expect(b).toBe(20);
      expect(c).toBe(30);
    });

    test("iterator protocol", () => {
      const ta = new TA([1, 2]);
      const iter = ta[Symbol.iterator]();
      const first = iter.next();
      expect(first.done).toBe(false);
      expect(first.value).toBe(1);
      const second = iter.next();
      expect(second.done).toBe(false);
      expect(second.value).toBe(2);
      expect(iter.next().done).toBe(true);
    });
  });

  test.each([BigInt64Array, BigUint64Array])("%s iteration", (TA) => {
    const ta = new TA([1n, 2n, 3n]);
    expect([...ta]).toEqual([1n, 2n, 3n]);
    const collected = [];
    for (const v of ta) collected.push(v);
    expect(collected).toEqual([1n, 2n, 3n]);
  });
});
