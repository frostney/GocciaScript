describe("Boolean.prototype.valueOf", () => {
  test("returns primitive boolean values", () => {
    expect(Boolean.prototype.valueOf.call(true)).toBe(true);
    expect(Boolean.prototype.valueOf.call(false)).toBe(false);
    expect(new Boolean(true).valueOf()).toBe(true);
  });

  test("rejects nullish receivers", () => {
    const valueOf = Boolean.prototype.valueOf;

    expect(() => valueOf.call(null)).toThrow(TypeError);
    expect(() => valueOf.call(undefined)).toThrow(TypeError);
    expect(() => valueOf()).toThrow(TypeError);
  });

  test("rejects non-Boolean receivers", () => {
    expect(() => Boolean.prototype.valueOf.call(1)).toThrow(TypeError);
    expect(() => Boolean.prototype.valueOf.call({})).toThrow(TypeError);
  });
});
