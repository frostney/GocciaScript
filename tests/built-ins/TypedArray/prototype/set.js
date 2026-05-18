describe("TypedArray.prototype.set", () => {
  test("cross-type set truncates", () => {
    const src = new Float64Array([1.5, 2.5, 3.5]);
    const dst = new Int32Array(3);
    dst.set(src);
    expect(dst[0]).toBe(1);
    expect(dst[1]).toBe(2);
    expect(dst[2]).toBe(3);
  });

  describe.each([Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float16Array, Float32Array, Float64Array])("%s", (TA) => {
    test("set from array", () => {
      const ta = new TA(3);
      ta.set([1, 2, 3]);
      expect(ta[0]).toBe(1);
      expect(ta[1]).toBe(2);
      expect(ta[2]).toBe(3);
    });

    test("set with offset", () => {
      const ta = new TA(5);
      ta.set([4, 5], 2);
      expect(ta[0]).toBe(0);
      expect(ta[2]).toBe(4);
      expect(ta[3]).toBe(5);
    });

    test("set from another typed array", () => {
      const src = new TA([7, 8, 9]);
      const dst = new TA(3);
      dst.set(src);
      expect(dst[0]).toBe(7);
      expect(dst[1]).toBe(8);
      expect(dst[2]).toBe(9);
    });

    test("negative offset throws RangeError", () => {
      const ta = new TA(4);
      expect(() => ta.set([1], -1)).toThrow(RangeError);
    });

    test("source too large throws RangeError", () => {
      const ta = new TA(2);
      expect(() => ta.set([1, 2, 3])).toThrow(RangeError);
    });

    test("offset causing overflow throws RangeError", () => {
      const ta = new TA(3);
      expect(() => ta.set([1, 2], 2)).toThrow(RangeError);
    });
  });

  describe("set from array-like", () => {
    test("set from plain array-like object", () => {
      const ta = new Int32Array(3);
      ta.set({ 0: 5, 1: 6, length: 2 });
      expect(ta[0]).toBe(5);
      expect(ta[1]).toBe(6);
      expect(ta[2]).toBe(0);
    });

    test("set from array-like with offset", () => {
      const ta = new Int32Array(4);
      ta.set({ 0: 10, 1: 20, length: 2 }, 2);
      expect(ta[0]).toBe(0);
      expect(ta[1]).toBe(0);
      expect(ta[2]).toBe(10);
      expect(ta[3]).toBe(20);
    });

    test("set from empty array-like", () => {
      const ta = new Int32Array([1, 2, 3]);
      ta.set({ length: 0 });
      expect(ta[0]).toBe(1);
      expect(ta[1]).toBe(2);
      expect(ta[2]).toBe(3);
    });

    test("array-like too large throws RangeError", () => {
      const ta = new Int32Array(2);
      expect(() => ta.set({ 0: 1, 1: 2, 2: 3, length: 3 })).toThrow(RangeError);
    });

    test("array-like with offset overflow throws RangeError", () => {
      const ta = new Int32Array(3);
      expect(() => ta.set({ 0: 1, 1: 2, length: 2 }, 2)).toThrow(RangeError);
    });

    test("array-like length getter can detach target buffer", () => {
      const ta = new Int32Array(4);
      const buffer = ta.buffer;
      ta.set({
        0: 17,
        get length() {
          buffer.transfer();
          return 1;
        }
      }, 1);

      expect(buffer.detached).toBe(true);
    });
  });

  test.each([BigInt64Array, BigUint64Array])("%s set from BigInt typed array", (TA) => {
    const src = new TA([10n, 20n]);
    const dst = new TA(4);
    dst.set(src, 1);
    expect(dst[0]).toBe(0n);
    expect(dst[1]).toBe(10n);
    expect(dst[2]).toBe(20n);
  });
});
