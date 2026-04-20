describe("TypedArray.prototype.sort", () => {
  test("on empty returns self", () => {
    const ta = new Int32Array(0);
    expect(ta.sort()).toBe(ta);
  });

  test("on single element is identity", () => {
    const ta = new Int32Array([42]);
    ta.sort();
    expect(ta[0]).toBe(42);
  });

  test("with negative numbers", () => {
    const ta = new Int32Array([3, -1, 4, -2, 0]);
    ta.sort();
    expect(ta[0]).toBe(-2);
    expect(ta[1]).toBe(-1);
    expect(ta[2]).toBe(0);
    expect(ta[3]).toBe(3);
    expect(ta[4]).toBe(4);
  });

  test("with duplicates", () => {
    const ta = new Int32Array([5, 3, 5, 1, 3]);
    ta.sort();
    expect(ta[0]).toBe(1);
    expect(ta[1]).toBe(3);
    expect(ta[2]).toBe(3);
    expect(ta[3]).toBe(5);
    expect(ta[4]).toBe(5);
  });

  test("already sorted array", () => {
    const ta = new Int32Array([1, 2, 3, 4, 5]);
    ta.sort();
    expect(ta[0]).toBe(1);
    expect(ta[4]).toBe(5);
  });

  test("reverse-sorted array", () => {
    const ta = new Int32Array([5, 4, 3, 2, 1]);
    ta.sort();
    expect(ta[0]).toBe(1);
    expect(ta[4]).toBe(5);
  });

  describe.each([Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float16Array, Float32Array, Float64Array])("%s", (TA) => {
    test("sorts numerically by default", () => {
      const ta = new TA([3, 1, 2]);
      ta.sort();
      expect(ta[0]).toBe(1);
      expect(ta[1]).toBe(2);
      expect(ta[2]).toBe(3);
    });

    test("sorts with comparator", () => {
      const ta = new TA([1, 2, 3]);
      ta.sort((a, b) => b - a);
      expect(ta[0]).toBe(3);
      expect(ta[1]).toBe(2);
      expect(ta[2]).toBe(1);
    });

    test("returns the typed array", () => {
      const ta = new TA([3, 1, 2]);
      expect(ta.sort()).toBe(ta);
    });
  });

  describe.each([BigInt64Array, BigUint64Array])("%s", (TA) => {
    test("default sort", () => {
      const ta = new TA([3n, 1n, 2n]);
      ta.sort();
      expect(ta[0]).toBe(1n);
      expect(ta[1]).toBe(2n);
      expect(ta[2]).toBe(3n);
    });

    test("sort with comparefn", () => {
      const ta = new TA([3n, 1n, 2n]);
      ta.sort((a, b) => {
        expect(typeof a).toBe("bigint");
        expect(typeof b).toBe("bigint");
        return a > b ? -1 : a < b ? 1 : 0;
      });
      expect(ta[0]).toBe(3n);
      expect(ta[1]).toBe(2n);
      expect(ta[2]).toBe(1n);
    });
  });
});
