describe("Set.prototype.intersection", () => {
  test("returns common elements", () => {
    const a = new Set([1, 2, 3, 4]);
    const b = new Set([3, 4, 5, 6]);
    const result = a.intersection(b);
    expect(result.size).toBe(2);
    expect(result.has(3)).toBe(true);
    expect(result.has(4)).toBe(true);
  });

  test("no common elements", () => {
    const a = new Set([1, 2]);
    const b = new Set([3, 4]);
    expect(a.intersection(b).size).toBe(0);
  });

  test("one empty set", () => {
    const a = new Set([1, 2]);
    expect(a.intersection(new Set()).size).toBe(0);
  });
});
