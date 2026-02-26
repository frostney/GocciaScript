describe("TypedArray.prototype.reduce", () => {
  test("sums elements", () => {
    const ta = new Int32Array([1, 2, 3, 4]);
    const sum = ta.reduce((acc, val) => acc + val, 0);
    expect(sum).toBe(10);
  });

  test("without initial value", () => {
    const ta = new Int32Array([1, 2, 3]);
    const sum = ta.reduce((acc, val) => acc + val);
    expect(sum).toBe(6);
  });

  test("on empty without initial value throws TypeError", () => {
    const ta = new Int32Array(0);
    expect(() => ta.reduce((acc, val) => acc + val)).toThrow(TypeError);
  });

  test("with initial value on empty array returns initial", () => {
    const ta = new Int32Array(0);
    expect(ta.reduce((acc, val) => acc + val, 99)).toBe(99);
  });

  test("single element with no initial value", () => {
    const ta = new Int32Array([42]);
    expect(ta.reduce((acc, val) => acc + val)).toBe(42);
  });

  test("without callback throws TypeError", () => {
    const ta = new Int32Array([1, 2, 3]);
    expect(() => ta.reduce()).toThrow(TypeError);
  });
});
