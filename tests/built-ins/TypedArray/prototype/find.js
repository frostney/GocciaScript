describe("TypedArray.prototype.find", () => {
  test("without callback throws TypeError", () => {
    const ta = new Int32Array([1, 2, 3]);
    expect(() => ta.find()).toThrow(TypeError);
  });

  test("passes thisArg to callback", () => {
    const ta = new Int32Array([1, 2, 3, 4, 5]);
    const ctx = { target: 4 };
    const obj = { fn(x) { return x === this.target; } };
    const found = ta.find(obj.fn, ctx);
    expect(found).toBe(4);
  });

  describe.each([Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float16Array, Float32Array, Float64Array])("%s", (TA) => {
    test("returns matching element", () => {
      const ta = new TA([1, 2, 3, 4]);
      expect(ta.find(x => x > 2)).toBe(3);
    });

    test("returns undefined when no match", () => {
      const ta = new TA([1, 2, 3]);
      expect(ta.find(x => x > 10)).toBeUndefined();
    });
  });

  test.each([BigInt64Array, BigUint64Array])("%s find", (TA) => {
    const ta = new TA([1n, 2n, 3n]);
    expect(ta.find(x => x > 1n)).toBe(2n);
  });
});
