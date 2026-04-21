describe("TypedArray.prototype.map", () => {
  describe.each([Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float16Array, Float32Array, Float64Array])("%s", (TA) => {
    test("maps elements", () => {
      const ta = new TA([1, 2, 3]);
      const mapped = ta.map(x => x * 2);
      expect(mapped.length).toBe(3);
      expect(mapped[0]).toBe(2);
      expect(mapped[1]).toBe(4);
      expect(mapped[2]).toBe(6);
    });

    test("returns instance of same type", () => {
      const ta = new TA([1, 2, 3]);
      const mapped = ta.map(x => x);
      expect(mapped).toBeInstanceOf(TA);
    });

    test("does not modify original", () => {
      const ta = new TA([1, 2, 3]);
      ta.map(x => x * 2);
      expect(ta[0]).toBe(1);
    });

    test("without callback throws TypeError", () => {
      const ta = new TA([1, 2, 3]);
      expect(() => ta.map()).toThrow(TypeError);
    });

    test("passes thisArg to callback", () => {
      const ta = new TA([1, 2, 3]);
      const ctx = { mult: 3 };
      const obj = { fn(x) { return x * this.mult; } };
      const mapped = ta.map(obj.fn, ctx);
      expect(mapped[0]).toBe(3);
      expect(mapped[1]).toBe(6);
      expect(mapped[2]).toBe(9);
    });
  });

  test.each([BigInt64Array, BigUint64Array])("%s map", (TA) => {
    const ta = new TA([1n, 2n, 3n]);
    const mapped = ta.map(x => x * 2n);
    expect(mapped[0]).toBe(2n);
    expect(mapped[1]).toBe(4n);
    expect(mapped[2]).toBe(6n);
  });
});
