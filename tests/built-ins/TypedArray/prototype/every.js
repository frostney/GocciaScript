describe("TypedArray.prototype.every", () => {
  test("returns true when all match", () => {
    const ta = new Int32Array([2, 4, 6]);
    expect(ta.every((x) => x % 2 === 0)).toBe(true);
  });

  test("returns false when one doesn't match", () => {
    const ta = new Int32Array([2, 3, 6]);
    expect(ta.every((x) => x % 2 === 0)).toBe(false);
  });

  test("on empty returns true", () => {
    const ta = new Int32Array(0);
    expect(ta.every((x) => x > 0)).toBe(true);
  });

  test("without callback throws TypeError", () => {
    const ta = new Int32Array([1, 2, 3]);
    expect(() => ta.every()).toThrow(TypeError);
  });
});
