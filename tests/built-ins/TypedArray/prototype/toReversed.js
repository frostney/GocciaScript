describe("TypedArray.prototype.toReversed", () => {
  test("returns new reversed array", () => {
    const ta = new Int32Array([1, 2, 3]);
    const rev = ta.toReversed();
    expect(rev[0]).toBe(3);
    expect(rev[1]).toBe(2);
    expect(rev[2]).toBe(1);
    expect(ta[0]).toBe(1);
  });

  test("on empty returns empty", () => {
    expect(new Int32Array(0).toReversed().length).toBe(0);
  });
});
