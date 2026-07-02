describe("Boolean.prototype.toString", () => {
  test("returns primitive boolean strings", () => {
    expect(Boolean.prototype.toString()).toBe("false");
    expect(Boolean.prototype.toString.call(true)).toBe("true");
    expect(Boolean.prototype.toString.call(false)).toBe("false");
    expect(new Boolean(true).toString()).toBe("true");
  });

  test("Boolean.prototype has Boolean wrapper internal data", () => {
    expect(Object.prototype.toString.call(Boolean.prototype)).toBe("[object Boolean]");

    const ownToString = Boolean.prototype.toString;
    delete Boolean.prototype.toString;
    try {
      expect(Boolean.prototype.toString()).toBe("[object Boolean]");
    } finally {
      Boolean.prototype.toString = ownToString;
    }
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
