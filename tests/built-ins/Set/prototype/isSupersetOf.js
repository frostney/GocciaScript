describe("Set.prototype.isSupersetOf", () => {
  test("true when contains all elements of other", () => {
    const a = new Set([1, 2, 3]);
    const b = new Set([1, 2]);
    expect(a.isSupersetOf(b)).toBe(true);
  });

  test("false when missing elements from other", () => {
    const a = new Set([1, 2]);
    const b = new Set([1, 2, 3]);
    expect(a.isSupersetOf(b)).toBe(false);
  });

  test("any set is superset of empty set", () => {
    expect(new Set([1]).isSupersetOf(new Set())).toBe(true);
    expect(new Set().isSupersetOf(new Set())).toBe(true);
  });

  test("throws TypeError when called on non-Set", () => {
    const isSupersetOf = Set.prototype.isSupersetOf;
    expect(() => isSupersetOf.call(Set.prototype, new Set())).toThrow(TypeError);
    expect(() => isSupersetOf.call({}, new Set())).toThrow(TypeError);
    expect(() => isSupersetOf.call(new Map(), new Set())).toThrow(TypeError);
  });

  test("throws TypeError when argument is not a Set", () => {
    const s = new Set([1, 2]);
    expect(() => s.isSupersetOf({})).toThrow(TypeError);
    expect(() => s.isSupersetOf([])).toThrow(TypeError);
  });
});
