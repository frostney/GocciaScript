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
    const result = new Set([1, 2, 3, 4]).difference(setLike);
    expect(result.size).toBe(2);
    expect(result.has(1)).toBe(true);
    expect(result.has(3)).toBe(true);
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
    const result = new Set([1, 2, 3, 4]).difference(setLike);
    expect(result.size).toBe(2);
    expect(result.has(1)).toBe(true);
    expect(result.has(3)).toBe(true);
  });

  test("throws TypeError when called on non-Set", () => {
    const difference = Set.prototype.difference;
    expect(() => difference.call(Set.prototype, new Set())).toThrow(TypeError);
    expect(() => difference.call({}, new Set())).toThrow(TypeError);
    expect(() => difference.call(new Map(), new Set())).toThrow(TypeError);
  });

  test("throws TypeError when argument is not set-like", () => {
    const s = new Set([1, 2]);
    expect(() => s.difference({})).toThrow(TypeError);
    expect(() => s.difference([])).toThrow(TypeError);
  });
});
