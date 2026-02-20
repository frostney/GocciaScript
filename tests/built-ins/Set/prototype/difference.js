describe("Set.prototype.difference", () => {
  test("returns elements in this but not other", () => {
    const a = new Set([1, 2, 3, 4]);
    const b = new Set([3, 4, 5]);
    const result = a.difference(b);
    expect(result.size).toBe(2);
    expect(result.has(1)).toBe(true);
    expect(result.has(2)).toBe(true);
  });

  test("no overlap returns all elements", () => {
    const a = new Set([1, 2]);
    const b = new Set([3, 4]);
    expect(a.difference(b).size).toBe(2);
  });

  test("complete overlap returns empty", () => {
    const a = new Set([1, 2]);
    const b = new Set([1, 2, 3]);
    expect(a.difference(b).size).toBe(0);
  });
});
