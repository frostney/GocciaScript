describe("TypedArray.prototype.map", () => {
  test("maps elements to new typed array", () => {
    const ta = new Int32Array([1, 2, 3]);
    const mapped = ta.map((x) => x * 2);
    expect(mapped.length).toBe(3);
    expect(mapped[0]).toBe(2);
    expect(mapped[1]).toBe(4);
    expect(mapped[2]).toBe(6);
  });

  test("does not modify original", () => {
    const ta = new Int32Array([1, 2, 3]);
    ta.map((x) => x * 2);
    expect(ta[0]).toBe(1);
  });

  test("without callback throws TypeError", () => {
    const ta = new Int32Array([1, 2, 3]);
    expect(() => ta.map()).toThrow(TypeError);
  });
});
