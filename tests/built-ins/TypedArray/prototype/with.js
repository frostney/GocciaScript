describe("TypedArray.prototype.with", () => {
  test("returns new array with element replaced", () => {
    const ta = new Int32Array([1, 2, 3]);
    const result = ta.with(1, 99);
    expect(result).not.toBe(ta);
    expect(result[0]).toBe(1);
    expect(result[1]).toBe(99);
    expect(result[2]).toBe(3);
    expect(ta[1]).toBe(2);
  });

  test("negative index", () => {
    const ta = new Int32Array([1, 2, 3]);
    const result = ta.with(-1, 99);
    expect(result[2]).toBe(99);
  });

  test("out of bounds throws RangeError", () => {
    const ta = new Int32Array([1, 2, 3]);
    expect(() => ta.with(3, 99)).toThrow(RangeError);
    expect(() => ta.with(-4, 99)).toThrow(RangeError);
  });

  test("without value uses undefined → NaN → 0 for integer types", () => {
    const ta = new Int32Array([1, 2, 3]);
    const result = ta.with(0);
    expect(result[0]).toBe(0);
    expect(result[1]).toBe(2);
    expect(result[2]).toBe(3);
  });

  test("without value uses undefined → NaN for float types", () => {
    const ta = new Float64Array([1.5, 2.5, 3.5]);
    const result = ta.with(0);
    expect(Number.isNaN(result[0])).toBe(true);
    expect(result[1]).toBe(2.5);
    expect(result[2]).toBe(3.5);
  });

  test("preserves type", () => {
    const ta = new Float64Array([1.5, 2.5, 3.5]);
    const result = ta.with(1, 9.9);
    expect(result instanceof Float64Array).toBe(true);
    expect(result[0]).toBe(1.5);
    expect(result[1]).toBe(9.9);
    expect(result[2]).toBe(3.5);
  });
});
