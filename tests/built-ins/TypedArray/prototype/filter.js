describe("TypedArray.prototype.filter", () => {
  describe.each([Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float16Array, Float32Array, Float64Array])("%s", (TA) => {
    test("filters elements", () => {
      const ta = new TA([1, 2, 3, 4, 5]);
      const filtered = ta.filter(x => x % 2 === 0);
      expect(filtered.length).toBe(2);
      expect(filtered[0]).toBe(2);
      expect(filtered[1]).toBe(4);
    });

    test("returns instance of same type", () => {
      const ta = new TA([1, 2, 3]);
      const filtered = ta.filter(x => x > 1);
      expect(filtered).toBeInstanceOf(TA);
    });

    test("no match returns empty", () => {
      const ta = new TA([1, 2, 3]);
      const filtered = ta.filter(x => x > 10);
      expect(filtered.length).toBe(0);
    });

    test("all elements match returns copy", () => {
      const ta = new TA([1, 2, 3]);
      const filtered = ta.filter(x => x > 0);
      expect(filtered.length).toBe(3);
      expect(filtered[0]).toBe(1);
      expect(filtered[1]).toBe(2);
      expect(filtered[2]).toBe(3);
    });

    test("without callback throws TypeError", () => {
      const ta = new TA([1, 2, 3]);
      expect(() => ta.filter()).toThrow(TypeError);
    });

    test("passes thisArg to callback", () => {
      const ta = new TA([1, 2, 3, 4, 5]);
      const ctx = { threshold: 3 };
      const obj = { fn(x) { return x >= this.threshold; } };
      const filtered = ta.filter(obj.fn, ctx);
      expect(filtered.length).toBe(3);
      expect(filtered[0]).toBe(3);
      expect(filtered[1]).toBe(4);
      expect(filtered[2]).toBe(5);
    });
  });

  test.each([BigInt64Array, BigUint64Array])("%s filter", (TA) => {
    const ta = new TA([1n, 2n, 3n, 4n]);
    const filtered = ta.filter(x => x > 2n);
    expect(filtered).toBeInstanceOf(TA);
    expect(filtered.length).toBe(2);
    expect(filtered[0]).toBe(3n);
    expect(filtered[1]).toBe(4n);
  });
});
