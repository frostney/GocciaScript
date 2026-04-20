describe("TypedArray.prototype.some", () => {
  describe.each([Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float16Array, Float32Array, Float64Array])("%s", (TA) => {
    test("returns true when one matches", () => {
      const ta = new TA([1, 2, 3]);
      expect(ta.some(x => x === 2)).toBe(true);
    });

    test("returns false when none match", () => {
      const ta = new TA([1, 3, 5]);
      expect(ta.some(x => x === 2)).toBe(false);
    });

    test("on empty returns false", () => {
      const ta = new TA(0);
      expect(ta.some(x => x > 0)).toBe(false);
    });

    test("stops iterating once callback returns true", () => {
      const ta = new TA([1, 2, 3, 4, 5]);
      const calls = [];
      ta.some((x, i) => {
        calls.push(i);
        return x === 3;
      });
      expect(calls).toEqual([0, 1, 2]);
    });

    test("callback receives (value, index, array)", () => {
      const ta = new TA([10]);
      let value, index, arr;
      ta.some((v, i, a) => {
        value = v;
        index = i;
        arr = a;
        return true;
      });
      expect(value).toBe(10);
      expect(index).toBe(0);
      expect(arr).toBe(ta);
    });

    test("without callback throws TypeError", () => {
      const ta = new TA([1, 2, 3]);
      expect(() => ta.some()).toThrow(TypeError);
    });

    test("non-callable callback throws TypeError", () => {
      const ta = new TA([1, 2, 3]);
      expect(() => ta.some(null)).toThrow(TypeError);
      expect(() => ta.some(42)).toThrow(TypeError);
    });

    test("passes thisArg to callback", () => {
      const ta = new TA([1, 2, 3]);
      const ctx = { target: 2 };
      const obj = { fn(x) { return x === this.target; } };
      expect(ta.some(obj.fn, ctx)).toBe(true);
    });
  });

  test.each([BigInt64Array, BigUint64Array])("%s some", (TA) => {
    const ta = new TA([1n, 2n, 3n]);
    expect(ta.some(x => x > 2n)).toBe(true);
    expect(ta.some(x => x > 5n)).toBe(false);
  });
});
