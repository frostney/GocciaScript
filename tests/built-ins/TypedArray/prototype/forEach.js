describe("TypedArray.prototype.forEach", () => {
  describe.each([Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float16Array, Float32Array, Float64Array])("%s", (TA) => {
    test("visits every element", () => {
      const ta = new TA([1, 2, 3]);
      const collected = [];
      ta.forEach(x => collected.push(x));
      expect(collected.length).toBe(3);
      expect(collected[0]).toBe(1);
      expect(collected[1]).toBe(2);
      expect(collected[2]).toBe(3);
    });

    test("returns undefined", () => {
      const ta = new TA([1, 2, 3]);
      const result = ta.forEach(() => {});
      expect(result).toBeUndefined();
    });

    test("without callback throws TypeError", () => {
      const ta = new TA([1, 2, 3]);
      expect(() => ta.forEach()).toThrow(TypeError);
    });

    test("passes thisArg to callback", () => {
      const ta = new TA([1, 2, 3]);
      const ctx = { sum: 0 };
      const obj = { fn(x) { this.sum += x; } };
      ta.forEach(obj.fn, ctx);
      expect(ctx.sum).toBe(6);
    });
  });

  test.each([BigInt64Array, BigUint64Array])("%s forEach", (TA) => {
    const ta = new TA([1n, 2n, 3n]);
    const collected = [];
    ta.forEach(x => collected.push(x));
    expect(collected[0]).toBe(1n);
    expect(collected[1]).toBe(2n);
    expect(collected[2]).toBe(3n);
  });
});
