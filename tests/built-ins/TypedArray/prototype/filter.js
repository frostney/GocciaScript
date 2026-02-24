describe("TypedArray.prototype.filter", () => {
  test("filters elements", () => {
    const ta = new Int32Array([1, 2, 3, 4, 5]);
    const filtered = ta.filter((x) => x % 2 === 0);
    expect(filtered.length).toBe(2);
    expect(filtered[0]).toBe(2);
    expect(filtered[1]).toBe(4);
  });

  test("no elements match returns empty typed array", () => {
    const ta = new Int32Array([1, 2, 3]);
    const filtered = ta.filter((x) => x > 10);
    expect(filtered.length).toBe(0);
  });

  test("all elements match returns copy", () => {
    const ta = new Int32Array([1, 2, 3]);
    const filtered = ta.filter((x) => x > 0);
    expect(filtered.length).toBe(3);
    expect(filtered[0]).toBe(1);
    expect(filtered[1]).toBe(2);
    expect(filtered[2]).toBe(3);
  });

  test("without callback throws TypeError", () => {
    const ta = new Int32Array([1, 2, 3]);
    expect(() => ta.filter()).toThrow(TypeError);
  });
});
