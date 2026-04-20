describe("TypedArray.prototype.findLastIndex", () => {
  test("returns last matching index", () => {
    const ta = new Int32Array([1, 2, 3, 4]);
    expect(ta.findLastIndex((x) => x < 3)).toBe(1);
  });

  test("returns -1 when no element satisfies the predicate", () => {
    const ta = new Int32Array([1, 2, 3]);
    expect(ta.findLastIndex((x) => x > 10)).toBe(-1);
  });

  test("without callback throws TypeError", () => {
    const ta = new Int32Array([1, 2, 3]);
    expect(() => ta.findLastIndex()).toThrow(TypeError);
  });

  test("passes thisArg to callback", () => {
    const ta = new Int32Array([1, 2, 3, 4, 5]);
    const ctx = { target: 4 };
    const obj = { fn(x) { return x === this.target; } };
    const idx = ta.findLastIndex(obj.fn, ctx);
    expect(idx).toBe(3);
  });

  describe.each([Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float16Array, Float32Array, Float64Array])("%s", (TA) => {
    test("returns last matching index", () => {
      const ta = new TA([1, 2, 3, 2]);
      expect(ta.findLastIndex(x => x === 2)).toBe(3);
    });

    test("returns -1 when no match", () => {
      const ta = new TA([1, 2, 3]);
      expect(ta.findLastIndex(x => x > 10)).toBe(-1);
    });
  });

  test.each([BigInt64Array, BigUint64Array])("%s findLastIndex", (TA) => {
    const ta = new TA([1n, 2n, 3n]);
    expect(ta.findLastIndex(x => x < 3n)).toBe(1);
  });
});
