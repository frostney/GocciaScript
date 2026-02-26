describe("TypedArray.prototype.at", () => {
  test("positive index", () => {
    const ta = new Int32Array([10, 20, 30]);
    expect(ta.at(0)).toBe(10);
    expect(ta.at(2)).toBe(30);
  });

  test("negative index", () => {
    const ta = new Int32Array([10, 20, 30]);
    expect(ta.at(-1)).toBe(30);
    expect(ta.at(-3)).toBe(10);
  });

  test("out of bounds returns undefined", () => {
    const ta = new Int32Array([10, 20, 30]);
    expect(ta.at(3)).toBeUndefined();
    expect(ta.at(-4)).toBeUndefined();
  });

  test("on empty returns undefined", () => {
    const ta = new Int32Array(0);
    expect(ta.at(0)).toBeUndefined();
  });
});
