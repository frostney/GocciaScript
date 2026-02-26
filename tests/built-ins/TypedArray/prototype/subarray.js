describe("TypedArray.prototype.subarray", () => {
  test("subarray shares the buffer", () => {
    const ta = new Int32Array([1, 2, 3, 4]);
    const sub = ta.subarray(1, 3);
    expect(sub.length).toBe(2);
    expect(sub[0]).toBe(2);
    expect(sub[1]).toBe(3);
    ta[1] = 99;
    expect(sub[0]).toBe(99);
  });

  test("subarray byteOffset reflects position", () => {
    const ta = new Int32Array([1, 2, 3, 4]);
    const sub = ta.subarray(2);
    expect(sub.byteOffset).toBe(8);
    expect(sub.length).toBe(2);
  });

  test("negative start", () => {
    const ta = new Int32Array([1, 2, 3, 4]);
    const sub = ta.subarray(-2);
    expect(sub.length).toBe(2);
    expect(sub[0]).toBe(3);
    expect(sub[1]).toBe(4);
  });

  test("begin > end returns empty view", () => {
    const ta = new Int32Array([1, 2, 3]);
    const sub = ta.subarray(2, 1);
    expect(sub.length).toBe(0);
  });

  test("both negative", () => {
    const ta = new Int32Array([1, 2, 3, 4, 5]);
    const sub = ta.subarray(-3, -1);
    expect(sub.length).toBe(2);
    expect(sub[0]).toBe(3);
    expect(sub[1]).toBe(4);
  });

  test("begin beyond length returns empty", () => {
    const ta = new Int32Array([1, 2, 3]);
    const sub = ta.subarray(10);
    expect(sub.length).toBe(0);
  });
});
