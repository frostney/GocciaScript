describe("TypedArray [Symbol.iterator]", () => {
  test("destructuring works", () => {
    const ta = new Int32Array([10, 20, 30]);
    const [a, b, c] = ta;
    expect(a).toBe(10);
    expect(b).toBe(20);
    expect(c).toBe(30);
  });

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
  });
});
