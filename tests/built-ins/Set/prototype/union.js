describe("Set.prototype.union", () => {
  test("combines elements from both sets", () => {
    const a = new Set([1, 2, 3]);
    const b = new Set([3, 4, 5]);
    const result = a.union(b);
    expect(result.size).toBe(5);
    expect(result.has(1)).toBe(true);
    expect(result.has(5)).toBe(true);
  });

  test("returns a new Set", () => {
    const a = new Set([1, 2]);
    const b = new Set([3]);
    const result = a.union(b);
    expect(result).not.toBe(a);
    expect(result).not.toBe(b);
  });

  test("union with empty set", () => {
    const a = new Set([1, 2]);
    const result = a.union(new Set());
    expect(result.size).toBe(2);
  });

  test("both empty", () => {
    const result = new Set().union(new Set());
    expect(result.size).toBe(0);
  });
});
