describe("TypedArray.prototype.keys", () => {
  test("iterates over indices", () => {
    const ta = new Int32Array([10, 20, 30]);
    const result = [];
    for (const k of ta.keys()) {
      result.push(k);
    }
    expect(result.length).toBe(3);
    expect(result[0]).toBe(0);
    expect(result[1]).toBe(1);
    expect(result[2]).toBe(2);
  });

  test("on empty returns done immediately", () => {
    const ta = new Int32Array(0);
    const iter = ta.keys();
    const result = iter.next();
    expect(result.done).toBe(true);
  });

  describe.each([Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float16Array, Float32Array, Float64Array])("%s", (TA) => {
    test("iterates over indices", () => {
      const ta = new TA([1, 2, 3]);
      const keys = [...ta.keys()];
      expect(keys).toEqual([0, 1, 2]);
    });

    test("empty array yields nothing", () => {
      const ta = new TA(0);
      const keys = [...ta.keys()];
      expect(keys).toEqual([]);
    });
  });
});
