describe("TypedArray.from", () => {
  test("from an array", () => {
    const ta = Int32Array.from([1, 2, 3]);
    expect(ta.length).toBe(3);
    expect(ta[0]).toBe(1);
    expect(ta[1]).toBe(2);
    expect(ta[2]).toBe(3);
  });

  test("from another typed array", () => {
    const src = new Float64Array([1.7, 2.3, 3.9]);
    const ta = Int32Array.from(src);
    expect(ta.length).toBe(3);
    expect(ta[0]).toBe(1);
    expect(ta[1]).toBe(2);
    expect(ta[2]).toBe(3);
  });

  test("from with map function", () => {
    const ta = Int32Array.from([1, 2, 3], (x) => x * 10);
    expect(ta[0]).toBe(10);
    expect(ta[1]).toBe(20);
    expect(ta[2]).toBe(30);
  });

  test("map function receives index", () => {
    const ta = Int32Array.from([10, 20, 30], (val, idx) => idx);
    expect(ta[0]).toBe(0);
    expect(ta[1]).toBe(1);
    expect(ta[2]).toBe(2);
  });

  test("from empty array creates empty typed array", () => {
    const ta = Int32Array.from([]);
    expect(ta.length).toBe(0);
  });

  test("without arguments throws TypeError", () => {
    expect(() => Int32Array.from()).toThrow(TypeError);
  });
});
