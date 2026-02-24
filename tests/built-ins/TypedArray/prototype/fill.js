describe("TypedArray.prototype.fill", () => {
  test("fill entire array", () => {
    const ta = new Int32Array(3);
    ta.fill(42);
    expect(ta[0]).toBe(42);
    expect(ta[1]).toBe(42);
    expect(ta[2]).toBe(42);
  });

  test("fill with start and end", () => {
    const ta = new Int32Array(5);
    ta.fill(7, 1, 4);
    expect(ta[0]).toBe(0);
    expect(ta[1]).toBe(7);
    expect(ta[2]).toBe(7);
    expect(ta[3]).toBe(7);
    expect(ta[4]).toBe(0);
  });

  test("returns the typed array", () => {
    const ta = new Int32Array(2);
    expect(ta.fill(1)).toBe(ta);
  });

  test("on empty returns self", () => {
    const ta = new Int32Array(0);
    expect(ta.fill(42)).toBe(ta);
  });

  test("negative start", () => {
    const ta = new Int32Array([1, 2, 3, 4, 5]);
    ta.fill(0, -2);
    expect(ta[0]).toBe(1);
    expect(ta[1]).toBe(2);
    expect(ta[2]).toBe(3);
    expect(ta[3]).toBe(0);
    expect(ta[4]).toBe(0);
  });

  test("negative start and end", () => {
    const ta = new Int32Array([1, 2, 3, 4, 5]);
    ta.fill(9, -3, -1);
    expect(ta[0]).toBe(1);
    expect(ta[1]).toBe(2);
    expect(ta[2]).toBe(9);
    expect(ta[3]).toBe(9);
    expect(ta[4]).toBe(5);
  });
});
