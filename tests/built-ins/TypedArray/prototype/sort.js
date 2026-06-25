describe("TypedArray.prototype.sort", () => {
  test("with negative numbers", () => {
    const ta = new Int32Array([3, -1, 4, -2, 0]);
    ta.sort();
    expect(ta[0]).toBe(-2);
    expect(ta[1]).toBe(-1);
    expect(ta[2]).toBe(0);
    expect(ta[3]).toBe(3);
    expect(ta[4]).toBe(4);
  });

  test("sorts a large array correctly (guards against the O(n^2) sort regression)", () => {
    const N = 60000;
    // Strictly descending input is the worst case for the previous insertion
    // sort; the bottom-up merge sort handles it in O(n log n).
    const ta = new Uint16Array(Array.from({ length: N }, (_, i) => N - i));
    ta.sort();
    expect(ta[0]).toBe(1);
    expect(ta[N - 1]).toBe(N);
    const out = Array.from(ta);
    expect(out.every((v, i) => i === 0 || out[i - 1] <= v)).toBe(true);
  });

  test("default sort orders -0 before +0 and places NaN last", () => {
    const ta = new Float64Array([NaN, 3, -0, 2, 0, 1, NaN]);
    ta.sort();
    expect(1 / ta[0]).toBe(-Infinity); // -0 sorts first
    expect(1 / ta[1]).toBe(Infinity); // +0 sorts second
    expect(ta[2]).toBe(1);
    expect(ta[3]).toBe(2);
    expect(ta[4]).toBe(3);
    expect(ta[5] !== ta[5]).toBe(true); // NaN
    expect(ta[6] !== ta[6]).toBe(true); // NaN
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

    test("on empty returns self", () => {
      const ta = new TA(0);
      expect(ta.sort()).toBe(ta);
    });

    test("on single element is identity", () => {
      const ta = new TA([5]);
      ta.sort();
      expect(ta[0]).toBe(5);
    });

    test("with duplicates", () => {
      const ta = new TA([5, 3, 5, 1, 3]);
      ta.sort();
      expect(ta[0]).toBe(1);
      expect(ta[1]).toBe(3);
      expect(ta[2]).toBe(3);
      expect(ta[3]).toBe(5);
      expect(ta[4]).toBe(5);
    });

    test("already sorted array", () => {
      const ta = new TA([1, 2, 3, 4, 5]);
      ta.sort();
      expect(ta[0]).toBe(1);
      expect(ta[4]).toBe(5);
    });

    test("reverse-sorted array", () => {
      const ta = new TA([5, 4, 3, 2, 1]);
      ta.sort();
      expect(ta[0]).toBe(1);
      expect(ta[4]).toBe(5);
    });
  });

  describe.each([Float16Array, Float32Array, Float64Array])("%s NaN and signed zero", (TA) => {
    test("NaN sorts to end", () => {
      const ta = new TA([NaN, 1, 2]);
      ta.sort();
      expect(ta[0]).toBe(1);
      expect(ta[1]).toBe(2);
      expect(Number.isNaN(ta[2])).toBe(true);
    });

    test("multiple NaN values sort to end", () => {
      const ta = new TA([NaN, 3, NaN, 1, 2]);
      ta.sort();
      expect(ta[0]).toBe(1);
      expect(ta[1]).toBe(2);
      expect(ta[2]).toBe(3);
      expect(Number.isNaN(ta[3])).toBe(true);
      expect(Number.isNaN(ta[4])).toBe(true);
    });

    test("-0 sorts before +0", () => {
      const ta = new TA([0, -0, 1]);
      ta.sort();
      expect(Object.is(ta[0], -0)).toBe(true);
      expect(Object.is(ta[1], 0)).toBe(true);
      expect(ta[2]).toBe(1);
    });

    test("-0 sorts before +0 when reversed", () => {
      const ta = new TA([1, -0, 0]);
      ta.sort();
      expect(Object.is(ta[0], -0)).toBe(true);
      expect(Object.is(ta[1], 0)).toBe(true);
      expect(ta[2]).toBe(1);
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
