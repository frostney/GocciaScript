describe("TypedArray.prototype.join", () => {
  test("single element has no separator", () => {
    expect(new Int32Array([42]).join()).toBe("42");
  });

  test("join with empty string separator", () => {
    expect(new Int32Array([1, 2, 3]).join("")).toBe("123");
  });

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
  });

  test.each([BigInt64Array, BigUint64Array])("%s join", (TA) => {
    const ta = new TA([1n, 2n, 3n]);
    expect(ta.join(",")).toBe("1,2,3");
  });
});
