describe("TypedArray.prototype.join", () => {
  test("Float64Array formats numbers", () => {
    expect(new Float64Array([1.5, 2.5]).join()).toBe("1.5,2.5");
  });

  describe.each([Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float16Array, Float32Array, Float64Array])("%s", (TA) => {
    test("joins with default separator", () => {
      const ta = new TA([1, 2, 3]);
      expect(ta.join()).toBe("1,2,3");
    });

    test("joins with custom separator", () => {
      const ta = new TA([1, 2, 3]);
      expect(ta.join("-")).toBe("1-2-3");
    });

    test("empty array returns empty string", () => {
      const ta = new TA(0);
      expect(ta.join()).toBe("");
    });

    test("single element has no separator", () => {
      const ta = new TA([5]);
      expect(ta.join()).toBe("5");
    });

    test("join with empty string separator", () => {
      const ta = new TA([1, 2, 3]);
      expect(ta.join("")).toBe("123");
    });

    test("Symbol separator throws TypeError", () => {
      const ta = new TA([1, 2]);
      expect(() => ta.join(Symbol())).toThrow(TypeError);
    });
  });

  test.each([BigInt64Array, BigUint64Array])("%s join", (TA) => {
    const ta = new TA([1n, 2n, 3n]);
    expect(ta.join(",")).toBe("1,2,3");
  });
});
