describe("TypedArray.prototype.slice", () => {
  test("slice entire array", () => {
    const ta = new Int32Array([1, 2, 3]);
    const sliced = ta.slice();
    expect(sliced.length).toBe(3);
    expect(sliced[0]).toBe(1);
    expect(sliced[2]).toBe(3);
  });

  test("slice with start", () => {
    const ta = new Int32Array([1, 2, 3, 4]);
    const sliced = ta.slice(1);
    expect(sliced.length).toBe(3);
    expect(sliced[0]).toBe(2);
  });

  test("slice with start and end", () => {
    const ta = new Int32Array([1, 2, 3, 4]);
    const sliced = ta.slice(1, 3);
    expect(sliced.length).toBe(2);
    expect(sliced[0]).toBe(2);
    expect(sliced[1]).toBe(3);
  });

  test("negative indices", () => {
    const ta = new Int32Array([1, 2, 3, 4]);
    const sliced = ta.slice(-2);
    expect(sliced.length).toBe(2);
    expect(sliced[0]).toBe(3);
    expect(sliced[1]).toBe(4);
  });

  test("returns new typed array of same type", () => {
    const ta = new Float32Array([1.5, 2.5]);
    const sliced = ta.slice();
    ta[0] = 99;
    expect(sliced[0]).toBe(1.5);
  });

  test("on empty returns empty", () => {
    const ta = new Int32Array(0);
    const sliced = ta.slice();
    expect(sliced.length).toBe(0);
  });

  test("start > end returns empty", () => {
    const ta = new Int32Array([1, 2, 3]);
    const sliced = ta.slice(2, 1);
    expect(sliced.length).toBe(0);
  });

  test("start beyond length returns empty", () => {
    const ta = new Int32Array([1, 2, 3]);
    const sliced = ta.slice(10);
    expect(sliced.length).toBe(0);
  });

  test("both negative", () => {
    const ta = new Int32Array([1, 2, 3, 4, 5]);
    const sliced = ta.slice(-3, -1);
    expect(sliced.length).toBe(2);
    expect(sliced[0]).toBe(3);
    expect(sliced[1]).toBe(4);
  });
});
