describe("TypedArray.prototype.toReversed", () => {
  test("throws TypeError for invalid receiver", () => {
    expect(() => Int32Array.prototype.toReversed.call(undefined)).toThrow(TypeError);
    expect(() => Int32Array.prototype.toReversed.call(null)).toThrow(TypeError);
    expect(() => Int32Array.prototype.toReversed.call({})).toThrow(TypeError);
    expect(() => Int32Array.prototype.toReversed.call(42)).toThrow(TypeError);
  });

  test("returns new reversed array", () => {
    const ta = new Int32Array([1, 2, 3]);
    const rev = ta.toReversed();
    expect(rev instanceof Int32Array).toBe(true);
    expect(rev[0]).toBe(3);
    expect(rev[1]).toBe(2);
    expect(rev[2]).toBe(1);
    expect(rev === ta).toBe(false);
    expect(ta[0]).toBe(1);
    expect(ta[1]).toBe(2);
    expect(ta[2]).toBe(3);
  });

  test("on empty returns empty", () => {
    expect(new Int32Array(0).toReversed().length).toBe(0);
  });
});
