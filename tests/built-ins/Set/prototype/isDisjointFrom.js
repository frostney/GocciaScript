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

  test("throws TypeError when called on non-Set", () => {
    const isDisjointFrom = Set.prototype.isDisjointFrom;
    expect(() => isDisjointFrom.call(Set.prototype, new Set())).toThrow(TypeError);
    expect(() => isDisjointFrom.call({}, new Set())).toThrow(TypeError);
    expect(() => isDisjointFrom.call(new Map(), new Set())).toThrow(TypeError);
  });

  test("throws TypeError when argument is not a Set", () => {
    const s = new Set([1, 2]);
    expect(() => s.isDisjointFrom({})).toThrow(TypeError);
    expect(() => s.isDisjointFrom([])).toThrow(TypeError);
  });
});
