describe("Set.prototype.isDisjointFrom", () => {
  test("true when no common elements", () => {
    const a = new Set([1, 2]);
    const b = new Set([3, 4]);
    expect(a.isDisjointFrom(b)).toBe(true);
  });

  test("false when common elements exist", () => {
    const a = new Set([1, 2, 3]);
    const b = new Set([3, 4, 5]);
    expect(a.isDisjointFrom(b)).toBe(false);
  });

  test("empty sets are disjoint", () => {
    expect(new Set().isDisjointFrom(new Set())).toBe(true);
  });

  test("empty set is disjoint from any set", () => {
    expect(new Set().isDisjointFrom(new Set([1, 2]))).toBe(true);
  });
});
