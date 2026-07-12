describe("Set.prototype.isSubsetOf", () => {
  test("true when all elements in other", () => {
    const a = new Set([1, 2]);
    const b = new Set([1, 2, 3]);
    expect(a.isSubsetOf(b)).toBe(true);
  });

  test("false when not all elements in other", () => {
    const a = new Set([1, 2, 4]);
    const b = new Set([1, 2, 3]);
    expect(a.isSubsetOf(b)).toBe(false);
  });

  test("empty set is subset of any set", () => {
    expect(new Set().isSubsetOf(new Set([1]))).toBe(true);
    expect(new Set().isSubsetOf(new Set())).toBe(true);
  });

  test("equal sets are subsets of each other", () => {
    const a = new Set([1, 2]);
    const b = new Set([1, 2]);
    expect(a.isSubsetOf(b)).toBe(true);
  });

  test("accepts set-like object", () => {
    const setLike = {
      size: 3,
      has(value) {
        return value === 1 || value === 2 || value === 3;
      },
      keys() {
        return [1, 2, 3].values();
      },
    };
    expect(new Set([1, 2]).isSubsetOf(setLike)).toBe(true);
    expect(new Set([1, 4]).isSubsetOf(setLike)).toBe(false);
  });

  test("visits members appended by the set-like has callback", () => {
    const receiver = new Set([1, 2, 3]);
    const additions = [4, 5];
    const seen = [];
    const setLike = {
      size: 5,
      has(value) {
        seen.push(value);
        receiver.delete(value);
        if (additions.length > 0) receiver.add(additions.shift());
        return true;
      },
      keys() {
        return [].values();
      },
    };
    expect(receiver.isSubsetOf(setLike)).toBe(true);
    expect(seen).toEqual([1, 2, 3, 4, 5]);
    expect(receiver.size).toBe(0);
  });

  test("throws TypeError when called on non-Set", () => {
    const isSubsetOf = Set.prototype.isSubsetOf;
    expect(() => isSubsetOf.call(Set.prototype, new Set())).toThrow(TypeError);
    expect(() => isSubsetOf.call({}, new Set())).toThrow(TypeError);
    expect(() => isSubsetOf.call(new Map(), new Set())).toThrow(TypeError);
  });

  test("throws TypeError when argument is not set-like", () => {
    const s = new Set([1, 2]);
    expect(() => s.isSubsetOf({})).toThrow(TypeError);
    expect(() => s.isSubsetOf([])).toThrow(TypeError);
  });
});
