describe("TypedArray.prototype.toString", () => {
  test("works like join", () => {
    const ta = new Int32Array([1, 2, 3]);
    expect(ta.toString()).toBe("1,2,3");
  });

  test("on empty returns empty string", () => {
    expect(new Int32Array(0).toString()).toBe("");
  });

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
