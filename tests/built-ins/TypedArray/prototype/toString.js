describe("TypedArray.prototype.toString", () => {
  describe.each([Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float16Array, Float32Array, Float64Array])("%s", (TA) => {
    test("converts to comma-separated string", () => {
      const ta = new TA([1, 2, 3]);
      expect(ta.toString()).toBe("1,2,3");
    });

    test("empty array returns empty string", () => {
      const ta = new TA(0);
      expect(ta.toString()).toBe("");
    });
  });
});
