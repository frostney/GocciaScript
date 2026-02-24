describe("TypedArray.prototype.some", () => {
  test("returns true when one matches", () => {
    const ta = new Int32Array([1, 2, 3]);
    expect(ta.some((x) => x === 2)).toBe(true);
  });

  test("returns false when none match", () => {
    const ta = new Int32Array([1, 3, 5]);
    expect(ta.some((x) => x === 2)).toBe(false);
  });

  test("on empty returns false", () => {
    const ta = new Int32Array(0);
    expect(ta.some((x) => x > 0)).toBe(false);
  });

  test("without callback throws TypeError", () => {
    const ta = new Int32Array([1, 2, 3]);
    expect(() => ta.some()).toThrow(TypeError);
  });

  test("passes thisArg to callback", () => {
    const ta = new Int32Array([1, 2, 3]);
    const ctx = { target: 2 };
    const obj = { fn(x) { return x === this.target; } };
    const hasTarget = ta.some(obj.fn, ctx);
    expect(hasTarget).toBe(true);
  });
});
