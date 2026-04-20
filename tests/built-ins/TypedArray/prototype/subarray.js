describe("TypedArray.prototype.subarray", () => {
  test("subarray shares the buffer", () => {
    const ta = new Int32Array([1, 2, 3, 4]);
    const sub = ta.subarray(1, 3);
    expect(sub.length).toBe(2);
    expect(sub[0]).toBe(2);
    expect(sub[1]).toBe(3);
    ta[1] = 99;
    expect(sub[0]).toBe(99);
  });

  test("subarray byteOffset reflects position", () => {
    const ta = new Int32Array([1, 2, 3, 4]);
    const sub = ta.subarray(2);
    expect(sub.byteOffset).toBe(8);
    expect(sub.length).toBe(2);
  });

  test("negative start", () => {
    const ta = new Int32Array([1, 2, 3, 4]);
    const sub = ta.subarray(-2);
    expect(sub.length).toBe(2);
    expect(sub[0]).toBe(3);
    expect(sub[1]).toBe(4);
  });

  test("begin > end returns empty view", () => {
    const ta = new Int32Array([1, 2, 3]);
    const sub = ta.subarray(2, 1);
    expect(sub.length).toBe(0);
  });

  test("both negative", () => {
    const ta = new Int32Array([1, 2, 3, 4, 5]);
    const sub = ta.subarray(-3, -1);
    expect(sub.length).toBe(2);
    expect(sub[0]).toBe(3);
    expect(sub[1]).toBe(4);
  });

  test("begin beyond length returns empty", () => {
    const ta = new Int32Array([1, 2, 3]);
    const sub = ta.subarray(10);
    expect(sub.length).toBe(0);
  });

  describe.each([Int8Array, Uint8Array, Uint8ClampedArray, Int16Array, Uint16Array, Int32Array, Uint32Array, Float16Array, Float32Array, Float64Array])("%s", (TA) => {
    test("subarray shares buffer", () => {
      const ta = new TA([1, 2, 3, 4, 5]);
      const sub = ta.subarray(1, 4);
      expect(sub.length).toBe(3);
      expect(sub[0]).toBe(2);
      expect(sub[1]).toBe(3);
      expect(sub[2]).toBe(4);
      sub[0] = 9;
      expect(ta[1]).toBe(9);
    });

    test("returns instance of same type", () => {
      const ta = new TA([1, 2, 3]);
      expect(ta.subarray(0, 2)).toBeInstanceOf(TA);
    });

    test("negative indices", () => {
      const ta = new TA([1, 2, 3, 4]);
      const sub = ta.subarray(-2);
      expect(sub.length).toBe(2);
      expect(sub[0]).toBe(3);
      expect(sub[1]).toBe(4);
    });
  });

  test.each([BigInt64Array, BigUint64Array])("%s subarray", (TA) => {
    const buf = new ArrayBuffer(32);
    const ta = new TA(buf);
    ta[0] = 10n; ta[1] = 20n; ta[2] = 30n; ta[3] = 40n;
    const sub = ta.subarray(1, 3);
    expect(sub.length).toBe(2);
    expect(sub[0]).toBe(20n);
    expect(sub[1]).toBe(30n);
    ta[1] = 99n;
    expect(sub[0]).toBe(99n);
  });
});
