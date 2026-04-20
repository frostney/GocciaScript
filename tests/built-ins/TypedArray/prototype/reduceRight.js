describe("TypedArray.prototype.reduceRight", () => {
  test("on empty without initial value throws TypeError", () => {
    const ta = new Int32Array(0);
    expect(() => ta.reduceRight((acc, val) => acc + val)).toThrow(TypeError);
  });

  test("single element with no initial value", () => {
    const ta = new Int32Array([42]);
    expect(ta.reduceRight((acc, val) => acc + val)).toBe(42);
  });

  test("without callback throws TypeError", () => {
    const ta = new Int32Array([1, 2, 3]);
    expect(() => ta.reduceRight()).toThrow(TypeError);
  });

  describe.each([Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float16Array, Float32Array, Float64Array])("%s", (TA) => {
    test("reduces from right with initial value", () => {
      const ta = new TA([1, 2, 3]);
      const sum = ta.reduceRight((acc, x) => acc + x, 0);
      expect(sum).toBe(6);
    });

    test("reduces from right without initial value", () => {
      const ta = new TA([1, 2, 3]);
      const result = ta.reduceRight((acc, x) => acc + x);
      expect(result).toBe(6);
    });

    test("on empty with initial value returns initial", () => {
      const ta = new TA(0);
      expect(ta.reduceRight((acc, x) => acc + x, 99)).toBe(99);
    });
  });

  test.each([BigInt64Array, BigUint64Array])("%s reduceRight", (TA) => {
    const ta = new TA([1n, 2n, 3n]);
    const result = ta.reduceRight((acc, x) => acc - x, 10n);
    expect(result).toBe(4n);
  });
});
