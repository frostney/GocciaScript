describe("TypedArray.prototype.some", () => {
  test("returns true when one matches", () => {
    const ta = new Int32Array([1, 2, 3]);
    expect(ta.some((x) => x === 2)).toBe(true);
  });

  test("callback receives (value, index, array)", () => {
    const ta = new Int32Array([10]);
    let value, index, arr;
    ta.some((v, i, a) => {
      value = v;
      index = i;
      arr = a;
      return true;
    });
    expect(value).toBe(10);
    expect(index).toBe(0);
    expect(arr).toBe(ta);
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

  test("non-callable callback throws TypeError", () => {
    const ta = new Int32Array([1, 2, 3]);
    expect(() => ta.some(null)).toThrow(TypeError);
    expect(() => ta.some(42)).toThrow(TypeError);
    expect(() => ta.some("str")).toThrow(TypeError);
  });

  test("passes thisArg to callback", () => {
    const ta = new Int32Array([1, 2, 3]);
    const ctx = { target: 2 };
    const obj = { fn(x) { return x === this.target; } };
    const hasTarget = ta.some(obj.fn, ctx);
    expect(hasTarget).toBe(true);
  });
});
