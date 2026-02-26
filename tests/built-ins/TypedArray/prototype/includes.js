describe("TypedArray.prototype.includes", () => {
  test("returns true when element exists", () => {
    const ta = new Int32Array([10, 20, 30]);
    expect(ta.includes(20)).toBe(true);
  });

  test("returns false when element does not exist", () => {
    const ta = new Int32Array([10, 20, 30]);
    expect(ta.includes(99)).toBe(false);
  });

  test("NaN handling with Float64Array", () => {
    const ta = new Float64Array([1, NaN, 3]);
    expect(ta.includes(NaN)).toBe(true);
  });

  test("on empty returns false", () => {
    expect(new Int32Array(0).includes(42)).toBe(false);
  });

  test("with fromIndex", () => {
    const ta = new Int32Array([1, 2, 3, 2]);
    expect(ta.includes(2, 2)).toBe(true);
    expect(ta.includes(2, 4)).toBe(false);
  });

  test("with negative fromIndex", () => {
    const ta = new Int32Array([1, 2, 3]);
    expect(ta.includes(1, -2)).toBe(false);
    expect(ta.includes(2, -2)).toBe(true);
  });

  test("0 vs -0 with Float64Array", () => {
    const ta = new Float64Array([0]);
    expect(ta.includes(-0)).toBe(true);
  });
});
