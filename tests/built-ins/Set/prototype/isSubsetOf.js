describe("Set.prototype.isSubsetOf", () => {
  test("true when all elements in other", () => {
    const a = new Set([1, 2]);
    const b = new Set([1, 2, 3]);
    expect(a.isSubsetOf(b)).toBe(true);
  });

  test("false when not all elements in other", () => {
    const a = new Set([1, 2, 4]);
    const b = new Set([1, 2, 3]);
    expect(a.isSubsetOf(b)).toBe(false);
  });

  test("empty set is subset of any set", () => {
    expect(new Set().isSubsetOf(new Set([1]))).toBe(true);
    expect(new Set().isSubsetOf(new Set())).toBe(true);
  });

  test("equal sets are subsets of each other", () => {
    const a = new Set([1, 2]);
    const b = new Set([1, 2]);
    expect(a.isSubsetOf(b)).toBe(true);
  });

  test("throws TypeError when called on non-Set", () => {
    const isSubsetOf = Set.prototype.isSubsetOf;
    expect(() => isSubsetOf.call(Set.prototype, new Set())).toThrow(TypeError);
    expect(() => isSubsetOf.call({}, new Set())).toThrow(TypeError);
    expect(() => isSubsetOf.call(new Map(), new Set())).toThrow(TypeError);
  });

  test("throws TypeError when argument is not a Set", () => {
    const s = new Set([1, 2]);
    expect(() => s.isSubsetOf({})).toThrow(TypeError);
    expect(() => s.isSubsetOf([])).toThrow(TypeError);
  });
});
