describe("TypedArray.prototype.reverse", () => {
  test("on empty returns self", () => {
    const ta = new Int32Array(0);
    expect(ta.reverse()).toBe(ta);
  });

  test("on single element is identity", () => {
    const ta = new Int32Array([42]);
    ta.reverse();
    expect(ta[0]).toBe(42);
  });

  describe.each([Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float16Array, Float32Array, Float64Array])("%s", (TA) => {
    test("reverses in place", () => {
      const ta = new TA([1, 2, 3]);
      ta.reverse();
      expect(ta[0]).toBe(3);
      expect(ta[1]).toBe(2);
      expect(ta[2]).toBe(1);
    });

    test("returns the typed array", () => {
      const ta = new TA([1, 2, 3]);
      expect(ta.reverse()).toBe(ta);
    });
  });

  test.each([BigInt64Array, BigUint64Array])("%s reverse", (TA) => {
    const ta = new TA([1n, 2n, 3n]);
    ta.reverse();
    expect(ta[0]).toBe(3n);
    expect(ta[1]).toBe(2n);
    expect(ta[2]).toBe(1n);
  });
});
