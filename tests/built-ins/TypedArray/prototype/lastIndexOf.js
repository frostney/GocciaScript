describe("TypedArray.prototype.lastIndexOf", () => {
  test("finds last occurrence", () => {
    const ta = new Int32Array([10, 20, 30, 20]);
    expect(ta.lastIndexOf(20)).toBe(3);
  });

  test("returns -1 when not found", () => {
    const ta = new Int32Array([10, 20, 30]);
    expect(ta.lastIndexOf(99)).toBe(-1);
  });

  test("fromIndex parameter", () => {
    const ta = new Int32Array([1, 2, 3, 2, 1]);
    expect(ta.lastIndexOf(2, 2)).toBe(1);
  });

  test("negative fromIndex", () => {
    const ta = new Int32Array([1, 2, 3, 2, 1]);
    expect(ta.lastIndexOf(2, -2)).toBe(3);
  });
});
