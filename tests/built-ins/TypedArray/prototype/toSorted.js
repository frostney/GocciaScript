describe("TypedArray.prototype.toSorted", () => {
  test("returns new sorted array", () => {
    const ta = new Int32Array([3, 1, 2]);
    const sorted = ta.toSorted();
    expect(sorted[0]).toBe(1);
    expect(sorted[1]).toBe(2);
    expect(sorted[2]).toBe(3);
    expect(ta[0]).toBe(3);
  });

  test("custom compare", () => {
    const ta = new Int32Array([1, 2, 3]);
    const sorted = ta.toSorted((a, b) => b - a);
    expect(sorted[0]).toBe(3);
    expect(sorted[1]).toBe(2);
    expect(sorted[2]).toBe(1);
  });

  test("on empty returns empty", () => {
    expect(new Int32Array(0).toSorted().length).toBe(0);
  });
});
