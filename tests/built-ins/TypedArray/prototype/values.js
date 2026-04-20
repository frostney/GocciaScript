describe("TypedArray.prototype.values", () => {
  test("iterates over values", () => {
    const ta = new Int32Array([10, 20, 30]);
    const result = [];
    for (const v of ta.values()) {
      result.push(v);
    }
    expect(result.length).toBe(3);
    expect(result[0]).toBe(10);
    expect(result[1]).toBe(20);
    expect(result[2]).toBe(30);
  });

  test("on empty returns done immediately", () => {
    const ta = new Int32Array(0);
    const iter = ta.values();
    const result = iter.next();
    expect(result.done).toBe(true);
  });

  describe.each([Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float16Array, Float32Array, Float64Array])("%s", (TA) => {
    test("iterates over values", () => {
      const ta = new TA([1, 2, 3]);
      const vals = [...ta.values()];
      expect(vals).toEqual([1, 2, 3]);
    });

    test("empty array yields nothing", () => {
      const ta = new TA(0);
      const vals = [...ta.values()];
      expect(vals).toEqual([]);
    });
  });

  test.each([BigInt64Array, BigUint64Array])("%s values iterator", (TA) => {
    const ta = new TA([10n, 20n]);
    const vals = [...ta.values()];
    expect(vals[0]).toBe(10n);
    expect(vals[1]).toBe(20n);
  });
});
