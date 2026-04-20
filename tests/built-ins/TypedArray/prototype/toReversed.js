describe("TypedArray.prototype.toReversed", () => {
  test("empty array returns empty", () => {
    expect(new Int32Array(0).toReversed().length).toBe(0);
  });

  test("single element", () => {
    const ta = new Int32Array([42]);
    const rev = ta.toReversed();
    expect(rev[0]).toBe(42);
    expect(rev.length).toBe(1);
  });

  describe.each([Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float16Array, Float32Array, Float64Array])("%s", (TA) => {
    test("returns reversed copy", () => {
      const ta = new TA([1, 2, 3]);
      const reversed = ta.toReversed();
      expect(reversed[0]).toBe(3);
      expect(reversed[1]).toBe(2);
      expect(reversed[2]).toBe(1);
    });

    test("does not modify original", () => {
      const ta = new TA([1, 2, 3]);
      ta.toReversed();
      expect(ta[0]).toBe(1);
    });

    test("returns instance of same type", () => {
      const ta = new TA([1, 2, 3]);
      expect(ta.toReversed()).toBeInstanceOf(TA);
    });
  });

  test.each([BigInt64Array, BigUint64Array])("%s toReversed", (TA) => {
    const ta = new TA([1n, 2n, 3n]);
    const rev = ta.toReversed();
    expect(rev[0]).toBe(3n);
    expect(rev[2]).toBe(1n);
    expect(ta[0]).toBe(1n);
  });
});
