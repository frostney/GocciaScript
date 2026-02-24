describe("TypedArray.prototype.set", () => {
  test("set from array", () => {
    const ta = new Int32Array(4);
    ta.set([10, 20, 30]);
    expect(ta[0]).toBe(10);
    expect(ta[1]).toBe(20);
    expect(ta[2]).toBe(30);
    expect(ta[3]).toBe(0);
  });

  test("set with offset", () => {
    const ta = new Int32Array(4);
    ta.set([10, 20], 2);
    expect(ta[0]).toBe(0);
    expect(ta[1]).toBe(0);
    expect(ta[2]).toBe(10);
    expect(ta[3]).toBe(20);
  });

  test("set from another typed array", () => {
    const src = new Float64Array([1.5, 2.5, 3.5]);
    const dst = new Int32Array(3);
    dst.set(src);
    expect(dst[0]).toBe(1);
    expect(dst[1]).toBe(2);
    expect(dst[2]).toBe(3);
  });

  test("source too large throws RangeError", () => {
    const ta = new Int32Array(2);
    expect(() => ta.set([1, 2, 3])).toThrow(RangeError);
  });

  test("set with typed array and offset causing overflow throws RangeError", () => {
    const ta = new Int32Array(3);
    const src = new Int32Array([1, 2, 3]);
    expect(() => ta.set(src, 1)).toThrow(RangeError);
  });

  test("set with array and offset causing overflow throws RangeError", () => {
    const ta = new Int32Array(3);
    expect(() => ta.set([1, 2], 2)).toThrow(RangeError);
  });
});
