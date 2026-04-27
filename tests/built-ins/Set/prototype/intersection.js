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

  test("accepts set-like object", () => {
    const setLike = {
      size: 10,
      has(value) {
        return value === 2 || value === 4;
      },
      keys() {
        return [2, 4].values();
      },
    };
    const result = new Set([1, 2, 3, 4]).intersection(setLike);
    expect(result.size).toBe(2);
    expect(result.has(2)).toBe(true);
    expect(result.has(4)).toBe(true);
  });

  test("uses set-like keys when other size is smaller", () => {
    const setLike = {
      size: 1,
      has(value) {
        return value === 2 || value === 4;
      },
      keys() {
        return [2, 4, 4].values();
      },
    };
    const result = new Set([1, 2, 3, 4]).intersection(setLike);
    expect(result.size).toBe(2);
    expect(result.has(2)).toBe(true);
    expect(result.has(4)).toBe(true);
  });

  test("throws TypeError when called on non-Set", () => {
    const intersection = Set.prototype.intersection;
    expect(() => intersection.call(Set.prototype, new Set())).toThrow(TypeError);
    expect(() => intersection.call({}, new Set())).toThrow(TypeError);
    expect(() => intersection.call(new Map(), new Set())).toThrow(TypeError);
  });

  test("throws TypeError when argument is not set-like", () => {
    const s = new Set([1, 2]);
    expect(() => s.intersection({})).toThrow(TypeError);
    expect(() => s.intersection([])).toThrow(TypeError);
  });
});
