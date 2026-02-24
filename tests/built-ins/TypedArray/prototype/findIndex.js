describe("TypedArray.prototype.findIndex", () => {
  test("returns matching index", () => {
    const ta = new Int32Array([1, 2, 3, 4]);
    expect(ta.findIndex((x) => x > 2)).toBe(2);
  });

  test("returns -1 when no match", () => {
    const ta = new Int32Array([1, 2, 3]);
    expect(ta.findIndex((x) => x > 10)).toBe(-1);
  });

  test("without callback throws TypeError", () => {
    const ta = new Int32Array([1, 2, 3]);
    expect(() => ta.findIndex()).toThrow(TypeError);
  });
});
