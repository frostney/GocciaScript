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

  test("skips an element deleted by a mutating has callback", () => {
    const a = new Set([1, 2, 3]);
    const setLike = {
      size: 5,
      has(value) {
        if (value === 1) a.delete(2);
        return true;
      },
      keys() {
        return [].values();
      },
    };
    const result = a.intersection(setLike);
    expect(result.has(1)).toBe(true);
    expect(result.has(3)).toBe(true);
    expect(result.has(2)).toBe(false);
    expect(result.size).toBe(2);
  });

  test("does not revisit a member deleted then re-added during a has callback", () => {
    const a = new Set([1, 2, 3]);
    const setLike = {
      size: 5,
      has(value) {
        if (value === 1) {
          a.delete(3);
          a.add(3);
        }
        return true;
      },
      keys() {
        return [].values();
      },
    };
    const result = a.intersection(setLike);
    // 3's original entry was emptied and the re-add is past the start-of-call
    // bound, so it is not visited.
    expect(result.has(1)).toBe(true);
    expect(result.has(2)).toBe(true);
    expect(result.has(3)).toBe(false);
    expect(result.size).toBe(2);
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
        throw new Error("has should not be called");
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
