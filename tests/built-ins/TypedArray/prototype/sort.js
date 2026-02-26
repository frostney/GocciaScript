describe("TypedArray.prototype.sort", () => {
  test("default numeric sort", () => {
    const ta = new Int32Array([3, 1, 4, 1, 5, 9]);
    ta.sort();
    expect(ta[0]).toBe(1);
    expect(ta[1]).toBe(1);
    expect(ta[2]).toBe(3);
    expect(ta[3]).toBe(4);
    expect(ta[4]).toBe(5);
    expect(ta[5]).toBe(9);
  });

  test("custom compare function", () => {
    const ta = new Int32Array([3, 1, 4, 1, 5]);
    ta.sort((a, b) => b - a);
    expect(ta[0]).toBe(5);
    expect(ta[1]).toBe(4);
    expect(ta[2]).toBe(3);
    expect(ta[3]).toBe(1);
    expect(ta[4]).toBe(1);
  });

  test("returns the typed array", () => {
    const ta = new Int32Array([2, 1]);
    expect(ta.sort()).toBe(ta);
  });

  test("on empty returns self", () => {
    const ta = new Int32Array(0);
    expect(ta.sort()).toBe(ta);
  });

  test("on single element is identity", () => {
    const ta = new Int32Array([42]);
    ta.sort();
    expect(ta[0]).toBe(42);
  });

  test("with negative numbers", () => {
    const ta = new Int32Array([3, -1, 4, -2, 0]);
    ta.sort();
    expect(ta[0]).toBe(-2);
    expect(ta[1]).toBe(-1);
    expect(ta[2]).toBe(0);
    expect(ta[3]).toBe(3);
    expect(ta[4]).toBe(4);
  });

  test("with duplicates", () => {
    const ta = new Int32Array([5, 3, 5, 1, 3]);
    ta.sort();
    expect(ta[0]).toBe(1);
    expect(ta[1]).toBe(3);
    expect(ta[2]).toBe(3);
    expect(ta[3]).toBe(5);
    expect(ta[4]).toBe(5);
  });

  test("already sorted array", () => {
    const ta = new Int32Array([1, 2, 3, 4, 5]);
    ta.sort();
    expect(ta[0]).toBe(1);
    expect(ta[4]).toBe(5);
  });

  test("reverse-sorted array", () => {
    const ta = new Int32Array([5, 4, 3, 2, 1]);
    ta.sort();
    expect(ta[0]).toBe(1);
    expect(ta[4]).toBe(5);
  });
});
