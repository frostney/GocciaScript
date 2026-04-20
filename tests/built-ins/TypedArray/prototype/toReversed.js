describe("TypedArray.prototype.toReversed", () => {
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
      expect(ta[1]).toBe(2);
      expect(ta[2]).toBe(3);
    });

    test("returns instance of same type", () => {
      const ta = new TA([1, 2, 3]);
      expect(ta.toReversed()).toBeInstanceOf(TA);
    });

    test("empty array returns empty", () => {
      const ta = new TA(0);
      expect(ta.toReversed().length).toBe(0);
    });

    test("single element", () => {
      const ta = new TA([5]);
      const rev = ta.toReversed();
      expect(rev[0]).toBe(5);
      expect(rev.length).toBe(1);
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
