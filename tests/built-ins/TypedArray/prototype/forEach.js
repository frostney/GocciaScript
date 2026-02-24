describe("TypedArray.prototype.forEach", () => {
  test("calls callback for each element", () => {
    const ta = new Int32Array([10, 20, 30]);
    const collected = [];
    ta.forEach((val, idx) => {
      collected.push([idx, val]);
    });
    expect(collected.length).toBe(3);
    expect(collected[0][0]).toBe(0);
    expect(collected[0][1]).toBe(10);
    expect(collected[2][1]).toBe(30);
  });

  test("returns undefined", () => {
    const ta = new Int32Array([1, 2, 3]);
    const result = ta.forEach((x) => x);
    expect(result).toBeUndefined();
  });

  test("without callback throws TypeError", () => {
    const ta = new Int32Array([1, 2, 3]);
    expect(() => ta.forEach()).toThrow(TypeError);
  });
});
