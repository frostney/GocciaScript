describe("TypedArray.prototype.find", () => {
  test("returns matching element", () => {
    const ta = new Int32Array([1, 2, 3, 4]);
    expect(ta.find((x) => x > 2)).toBe(3);
  });

  test("returns undefined when no match", () => {
    const ta = new Int32Array([1, 2, 3]);
    expect(ta.find((x) => x > 10)).toBeUndefined();
  });

  test("without callback throws TypeError", () => {
    const ta = new Int32Array([1, 2, 3]);
    expect(() => ta.find()).toThrow(TypeError);
  });

  test("passes thisArg to callback", () => {
    const ta = new Int32Array([1, 2, 3, 4, 5]);
    const ctx = { target: 4 };
    const obj = { fn(x) { return x === this.target; } };
    const found = ta.find(obj.fn, ctx);
    expect(found).toBe(4);
  });
});
