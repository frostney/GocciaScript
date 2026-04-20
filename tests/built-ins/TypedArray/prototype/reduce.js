describe("TypedArray.prototype.reduce", () => {
  describe.each([Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float16Array, Float32Array, Float64Array])("%s", (TA) => {
    test("sums elements with initial value", () => {
      const ta = new TA([1, 2, 3]);
      const sum = ta.reduce((acc, x) => acc + x, 0);
      expect(sum).toBe(6);
    });

    test("without initial value", () => {
      const ta = new TA([1, 2, 3]);
      const sum = ta.reduce((acc, x) => acc + x);
      expect(sum).toBe(6);
    });

    test("on empty with initial value returns initial", () => {
      const ta = new TA(0);
      expect(ta.reduce((acc, x) => acc + x, 99)).toBe(99);
    });

    test("on empty without initial value throws TypeError", () => {
      const ta = new TA(0);
      expect(() => ta.reduce((acc, x) => acc + x)).toThrow(TypeError);
    });

    test("single element with no initial value", () => {
      const ta = new TA([5]);
      expect(ta.reduce((acc, x) => acc + x)).toBe(5);
    });

    test("without callback throws TypeError", () => {
      const ta = new TA([1, 2, 3]);
      expect(() => ta.reduce()).toThrow(TypeError);
    });
  });

  test.each([BigInt64Array, BigUint64Array])("%s reduce", (TA) => {
    const ta = new TA([1n, 2n, 3n]);
    const sum = ta.reduce((acc, x) => acc + x, 0n);
    expect(sum).toBe(6n);
  });
});
