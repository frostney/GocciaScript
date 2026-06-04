describe("Boolean.prototype.toString", () => {
  test("returns primitive boolean strings", () => {
    expect(Boolean.prototype.toString.call(true)).toBe("true");
    expect(Boolean.prototype.toString.call(false)).toBe("false");
    expect(new Boolean(true).toString()).toBe("true");
  });

  test("rejects nullish receivers", () => {
    const toString = Boolean.prototype.toString;

    expect(() => toString.call(null)).toThrow(TypeError);
    expect(() => toString.call(undefined)).toThrow(TypeError);
    expect(() => toString()).toThrow(TypeError);
  });

  test("rejects non-Boolean receivers", () => {
    expect(() => Boolean.prototype.toString.call(1)).toThrow(TypeError);
    expect(() => Boolean.prototype.toString.call({})).toThrow(TypeError);
  });
});
