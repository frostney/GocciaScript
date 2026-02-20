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
});
