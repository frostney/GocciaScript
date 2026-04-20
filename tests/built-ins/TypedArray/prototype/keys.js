describe("TypedArray.prototype.keys", () => {
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
