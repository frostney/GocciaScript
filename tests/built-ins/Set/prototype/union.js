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

  test("accepts set-like object", () => {
    const setLike = {
      size: "3",
      has(value) {
        return value === 2 || value === 3;
      },
      keys() {
        return [2, 3, 3].values();
      },
    };
    const result = new Set([1, 2]).union(setLike);
    expect(result.size).toBe(3);
    expect(result.has(1)).toBe(true);
    expect(result.has(2)).toBe(true);
    expect(result.has(3)).toBe(true);
  });

  test("throws TypeError when called on non-Set", () => {
    const union = Set.prototype.union;
    expect(() => union.call(Set.prototype, new Set())).toThrow(TypeError);
    expect(() => union.call({}, new Set())).toThrow(TypeError);
    expect(() => union.call(new Map(), new Set())).toThrow(TypeError);
  });

  test("throws TypeError when argument is not set-like", () => {
    const s = new Set([1, 2]);
    expect(() => s.union({})).toThrow(TypeError);
    expect(() => s.union([])).toThrow(TypeError);
  });

  test("validates set-like object fields", () => {
    const s = new Set([1, 2]);
    expect(() => s.union({
      has(value) {
        return value === 1;
      },
      keys() {
        return [1].values();
      },
    })).toThrow(TypeError);
    expect(() => s.union({
      size: -1,
      has(value) {
        return value === 1;
      },
      keys() {
        return [1].values();
      },
    })).toThrow(RangeError);
    expect(() => s.union({
      size: 1,
      has: 1,
      keys() {
        return [1].values();
      },
    })).toThrow(TypeError);
    expect(() => s.union({
      size: 1,
      has(value) {
        return value === 1;
      },
      keys: 1,
    })).toThrow(TypeError);
    expect(() => s.union({
      size: 1,
      has(value) {
        return value === 1;
      },
      keys() {
        return {};
      },
    })).toThrow(TypeError);
  });
});
