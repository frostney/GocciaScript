describe("TypedArray.prototype.findLast", () => {
  test("without callback throws TypeError", () => {
    const ta = new Int32Array([1, 2, 3]);
    expect(() => ta.findLast()).toThrow(TypeError);
  });

  test("passes thisArg to callback", () => {
    const ta = new Int32Array([1, 2, 3, 4, 5]);
    const ctx = { target: 4 };
    const obj = { fn(x) { return x === this.target; } };
    const found = ta.findLast(obj.fn, ctx);
    expect(found).toBe(4);
  });

  describe.each([Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float16Array, Float32Array, Float64Array])("%s", (TA) => {
    test("returns last matching element", () => {
      const ta = new TA([1, 2, 3, 4]);
      expect(ta.findLast(x => x % 2 === 0)).toBe(4);
    });

    test("returns undefined when no match", () => {
      const ta = new TA([1, 2, 3]);
      expect(ta.findLast(x => x > 10)).toBeUndefined();
    });
  });

  test.each([BigInt64Array, BigUint64Array])("%s findLast", (TA) => {
    const ta = new TA([1n, 2n, 3n]);
    expect(ta.findLast(x => x < 3n)).toBe(2n);
  });
});
