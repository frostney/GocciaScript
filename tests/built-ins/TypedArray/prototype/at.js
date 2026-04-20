describe("TypedArray.prototype.at", () => {
  describe.each([Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float16Array, Float32Array, Float64Array])("%s", (TA) => {
    test("positive index", () => {
      const ta = new TA([1, 2, 3]);
      expect(ta.at(0)).toBe(1);
      expect(ta.at(2)).toBe(3);
    });

    test("negative index", () => {
      const ta = new TA([1, 2, 3]);
      expect(ta.at(-1)).toBe(3);
      expect(ta.at(-3)).toBe(1);
    });

    test("out of bounds returns undefined", () => {
      const ta = new TA([1, 2, 3]);
      expect(ta.at(3)).toBeUndefined();
      expect(ta.at(-4)).toBeUndefined();
    });

    test("on empty returns undefined", () => {
      const ta = new TA(0);
      expect(ta.at(0)).toBeUndefined();
    });
  });

  test.each([BigInt64Array, BigUint64Array])("%s returns BigInt values", (TA) => {
    const ta = new TA([10n, 20n, 30n]);
    expect(ta.at(0)).toBe(10n);
    expect(ta.at(-1)).toBe(30n);
  });
});
