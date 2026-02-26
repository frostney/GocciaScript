describe("TypedArray.prototype.reverse", () => {
  test("reverses in place", () => {
    const ta = new Int32Array([1, 2, 3, 4]);
    ta.reverse();
    expect(ta[0]).toBe(4);
    expect(ta[1]).toBe(3);
    expect(ta[2]).toBe(2);
    expect(ta[3]).toBe(1);
  });

  test("returns the typed array", () => {
    const ta = new Int32Array([1, 2]);
    expect(ta.reverse()).toBe(ta);
  });

  test("on empty returns self", () => {
    const ta = new Int32Array(0);
    expect(ta.reverse()).toBe(ta);
  });

  test("on single element is identity", () => {
    const ta = new Int32Array([42]);
    ta.reverse();
    expect(ta[0]).toBe(42);
  });
});
