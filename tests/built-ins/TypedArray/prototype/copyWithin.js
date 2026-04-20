describe("TypedArray.prototype.copyWithin", () => {
  test("copy elements within the array", () => {
    const ta = new Int32Array([1, 2, 3, 4, 5]);
    ta.copyWithin(0, 3);
    expect(ta[0]).toBe(4);
    expect(ta[1]).toBe(5);
    expect(ta[2]).toBe(3);
    expect(ta[3]).toBe(4);
    expect(ta[4]).toBe(5);
  });

  test("copy with end parameter", () => {
    const ta = new Int32Array([1, 2, 3, 4, 5]);
    ta.copyWithin(1, 3, 4);
    expect(ta[0]).toBe(1);
    expect(ta[1]).toBe(4);
    expect(ta[2]).toBe(3);
  });

  test("returns the typed array", () => {
    const ta = new Int32Array([1, 2, 3]);
    expect(ta.copyWithin(0, 1)).toBe(ta);
  });

  test("negative target", () => {
    const ta = new Int32Array([1, 2, 3, 4, 5]);
    ta.copyWithin(-2, 0, 2);
    expect(ta[3]).toBe(1);
    expect(ta[4]).toBe(2);
  });

  test("negative source", () => {
    const ta = new Int32Array([1, 2, 3, 4, 5]);
    ta.copyWithin(0, -2);
    expect(ta[0]).toBe(4);
    expect(ta[1]).toBe(5);
  });

  describe.each([Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float16Array, Float32Array, Float64Array])("%s", (TA) => {
    test("copies within the array", () => {
      const ta = new TA([1, 2, 3, 4, 5]);
      ta.copyWithin(0, 3);
      expect(ta[0]).toBe(4);
      expect(ta[1]).toBe(5);
      expect(ta[2]).toBe(3);
    });

    test("copies with start and end", () => {
      const ta = new TA([1, 2, 3, 4, 5]);
      ta.copyWithin(1, 3, 4);
      expect(ta[0]).toBe(1);
      expect(ta[1]).toBe(4);
      expect(ta[2]).toBe(3);
    });

    test("returns the typed array", () => {
      const ta = new TA([1, 2, 3]);
      expect(ta.copyWithin(0, 1)).toBe(ta);
    });
  });

  test.each([BigInt64Array, BigUint64Array])("%s copyWithin", (TA) => {
    const ta = new TA([1n, 2n, 3n, 4n, 5n]);
    ta.copyWithin(0, 3);
    expect(ta[0]).toBe(4n);
    expect(ta[1]).toBe(5n);
    expect(ta[2]).toBe(3n);
    expect(ta[3]).toBe(4n);
    expect(ta[4]).toBe(5n);
  });
});
