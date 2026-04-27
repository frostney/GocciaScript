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

  test("throws TypeError when called on non-Set", () => {
    const symDiff = Set.prototype.symmetricDifference;
    expect(() => symDiff.call(Set.prototype, new Set())).toThrow(TypeError);
    expect(() => symDiff.call({}, new Set())).toThrow(TypeError);
    expect(() => symDiff.call(new Map(), new Set())).toThrow(TypeError);
  });

  test("throws TypeError when argument is not a Set", () => {
    const s = new Set([1, 2]);
    expect(() => s.symmetricDifference({})).toThrow(TypeError);
    expect(() => s.symmetricDifference([])).toThrow(TypeError);
  });
});
