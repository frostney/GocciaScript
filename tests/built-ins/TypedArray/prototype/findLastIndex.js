describe("TypedArray.prototype.findLastIndex", () => {
  test("returns last matching index", () => {
    const ta = new Int32Array([1, 2, 3, 4]);
    expect(ta.findLastIndex((x) => x < 3)).toBe(1);
  });

  test("without callback throws TypeError", () => {
    const ta = new Int32Array([1, 2, 3]);
    expect(() => ta.findLastIndex()).toThrow(TypeError);
  });
});
