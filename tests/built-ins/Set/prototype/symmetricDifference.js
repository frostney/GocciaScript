describe("Set.prototype.symmetricDifference", () => {
  test("returns elements in either but not both", () => {
    const a = new Set([1, 2, 3]);
    const b = new Set([2, 3, 4]);
    const result = a.symmetricDifference(b);
    expect(result.size).toBe(2);
    expect(result.has(1)).toBe(true);
    expect(result.has(4)).toBe(true);
  });

  test("no overlap returns all elements", () => {
    const a = new Set([1, 2]);
    const b = new Set([3, 4]);
    expect(a.symmetricDifference(b).size).toBe(4);
  });

  test("identical sets return empty", () => {
    const a = new Set([1, 2]);
    const b = new Set([1, 2]);
    expect(a.symmetricDifference(b).size).toBe(0);
  });
});
