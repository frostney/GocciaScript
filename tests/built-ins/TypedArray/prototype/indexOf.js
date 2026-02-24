describe("TypedArray.prototype.indexOf", () => {
  test("finds element", () => {
    const ta = new Int32Array([10, 20, 30, 20]);
    expect(ta.indexOf(20)).toBe(1);
  });

  test("returns -1 when not found", () => {
    const ta = new Int32Array([10, 20, 30]);
    expect(ta.indexOf(99)).toBe(-1);
  });

  test("fromIndex parameter", () => {
    const ta = new Int32Array([10, 20, 30, 20]);
    expect(ta.indexOf(20, 2)).toBe(3);
  });

  test("on empty returns -1", () => {
    expect(new Int32Array(0).indexOf(42)).toBe(-1);
  });

  test("negative fromIndex", () => {
    const ta = new Int32Array([1, 2, 3, 2, 1]);
    expect(ta.indexOf(2, -3)).toBe(3);
  });

  test("fromIndex beyond length returns -1", () => {
    const ta = new Int32Array([1, 2, 3]);
    expect(ta.indexOf(1, 10)).toBe(-1);
  });
});
