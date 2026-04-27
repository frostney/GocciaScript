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

  test("accepts set-like object", () => {
    const setLike = {
      size: 10,
      has(value) {
        return value === 3 || value === 4;
      },
      keys() {
        return [3, 4].values();
      },
    };
    expect(new Set([1, 2]).isDisjointFrom(setLike)).toBe(true);
    expect(new Set([2, 3]).isDisjointFrom(setLike)).toBe(false);
  });

  test("uses set-like keys when other size is smaller", () => {
    let hasCalls = 0;
    const setLike = {
      size: 1,
      has(value) {
        hasCalls = hasCalls + 1;
        throw new Error("isDisjointFrom should use keys() when other size is smaller");
      },
      keys() {
        return [3, 4].values();
      },
    };
    expect(new Set([1, 2]).isDisjointFrom(setLike)).toBe(true);
    expect(new Set([2, 3]).isDisjointFrom(setLike)).toBe(false);
    expect(hasCalls).toBe(0);
  });

  test("throws TypeError when called on non-Set", () => {
    const isDisjointFrom = Set.prototype.isDisjointFrom;
    expect(() => isDisjointFrom.call(Set.prototype, new Set())).toThrow(TypeError);
    expect(() => isDisjointFrom.call({}, new Set())).toThrow(TypeError);
    expect(() => isDisjointFrom.call(new Map(), new Set())).toThrow(TypeError);
  });

  test("throws TypeError when argument is not set-like", () => {
    const s = new Set([1, 2]);
    expect(() => s.isDisjointFrom({})).toThrow(TypeError);
    expect(() => s.isDisjointFrom([])).toThrow(TypeError);
  });
});
