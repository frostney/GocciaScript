describe("TypedArray.prototype.findLast", () => {
  test("returns last matching element", () => {
    const ta = new Int32Array([1, 2, 3, 4]);
    expect(ta.findLast((x) => x < 3)).toBe(2);
  });

  test("without callback throws TypeError", () => {
    const ta = new Int32Array([1, 2, 3]);
    expect(() => ta.findLast()).toThrow(TypeError);
  });
});
