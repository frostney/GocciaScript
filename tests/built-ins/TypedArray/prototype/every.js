describe("TypedArray.prototype.every", () => {
  test("without callback throws TypeError", () => {
    const ta = new Int32Array([1, 2, 3]);
    expect(() => ta.every()).toThrow(TypeError);
  });

  test("passes thisArg to callback", () => {
    const ta = new Int32Array([2, 4, 6]);
    const ctx = { divisor: 2 };
    const obj = { fn(x) { return x % this.divisor === 0; } };
    const allEven = ta.every(obj.fn, ctx);
    expect(allEven).toBe(true);
  });

  describe.each([Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float16Array, Float32Array, Float64Array])("%s", (TA) => {
    test("returns true when all match", () => {
      const ta = new TA([2, 4, 6]);
      expect(ta.every(x => x % 2 === 0)).toBe(true);
    });

    test("returns false when one doesn't match", () => {
      const ta = new TA([2, 3, 6]);
      expect(ta.every(x => x % 2 === 0)).toBe(false);
    });

    test("on empty returns true", () => {
      const ta = new TA(0);
      expect(ta.every(x => x > 0)).toBe(true);
    });
  });

  test.each([BigInt64Array, BigUint64Array])("%s every", (TA) => {
    const ta = new TA([2n, 4n, 6n]);
    expect(ta.every(x => x % 2n === 0n)).toBe(true);
    expect(ta.every(x => x > 3n)).toBe(false);
  });
});
