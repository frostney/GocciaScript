/*---
description: Iterator.prototype helper methods (map, filter, reduce, forEach, etc.)
features: [Iterator, Iterator.prototype.map, Iterator.prototype.filter, Iterator.prototype.reduce]
---*/

describe("Iterator.prototype.map()", () => {
  test("maps values through a callback", () => {
    const result = [1, 2, 3].values().map((x) => x * 2).toArray();
    expect(result).toEqual([2, 4, 6]);
  });

  test("map on empty iterator", () => {
    const result = [].values().map((x) => x * 2).toArray();
    expect(result).toEqual([]);
  });

  test("map is lazy - returns an iterator, not an array", () => {
    const mapped = [1, 2, 3].values().map((x) => x * 2);
    expect(mapped.next().value).toBe(2);
    expect(mapped.next().value).toBe(4);
    expect(mapped.next().value).toBe(6);
    expect(mapped.next().done).toBe(true);
  });
});

describe("Iterator.prototype.filter()", () => {
  test("filters values by predicate", () => {
    const result = [1, 2, 3, 4, 5].values().filter((x) => x > 3).toArray();
    expect(result).toEqual([4, 5]);
  });

  test("filter with no matches", () => {
    const result = [1, 2, 3].values().filter((x) => x > 10).toArray();
    expect(result).toEqual([]);
  });

  test("filter is lazy - returns an iterator, not an array", () => {
    const filtered = [1, 2, 3, 4, 5].values().filter((x) => x % 2 === 0);
    expect(filtered.next().value).toBe(2);
    expect(filtered.next().value).toBe(4);
    expect(filtered.next().done).toBe(true);
  });
});

describe("Iterator.prototype.take()", () => {
  test("takes first N elements", () => {
    const result = [1, 2, 3, 4, 5].values().take(3).toArray();
    expect(result).toEqual([1, 2, 3]);
  });

  test("take more than available", () => {
    const result = [1, 2].values().take(5).toArray();
    expect(result).toEqual([1, 2]);
  });

  test("take zero", () => {
    const result = [1, 2, 3].values().take(0).toArray();
    expect(result).toEqual([]);
  });

  test("take is lazy - only consumes needed elements", () => {
    const taken = [1, 2, 3, 4, 5].values().take(2);
    expect(taken.next().value).toBe(1);
    expect(taken.next().value).toBe(2);
    expect(taken.next().done).toBe(true);
  });
});

describe("Iterator.prototype.drop()", () => {
  test("drops first N elements", () => {
    const result = [1, 2, 3, 4, 5].values().drop(2).toArray();
    expect(result).toEqual([3, 4, 5]);
  });

  test("drop all elements", () => {
    const result = [1, 2, 3].values().drop(5).toArray();
    expect(result).toEqual([]);
  });

  test("drop zero", () => {
    const result = [1, 2, 3].values().drop(0).toArray();
    expect(result).toEqual([1, 2, 3]);
  });

  test("drop is lazy - skips on first next() call", () => {
    const dropped = [1, 2, 3, 4, 5].values().drop(3);
    expect(dropped.next().value).toBe(4);
    expect(dropped.next().value).toBe(5);
    expect(dropped.next().done).toBe(true);
  });
});

describe("Iterator.prototype.forEach()", () => {
  test("calls callback for each value", () => {
    const collected = [];
    [1, 2, 3].values().forEach((x) => collected.push(x));
    expect(collected).toEqual([1, 2, 3]);
  });

  test("forEach returns undefined", () => {
    const result = [1].values().forEach((x) => x);
    expect(result).toBe(undefined);
  });
});

describe("Iterator.prototype.reduce()", () => {
  test("reduces with initial value", () => {
    const result = [1, 2, 3].values().reduce((acc, x) => acc + x, 0);
    expect(result).toBe(6);
  });

  test("reduces without initial value uses first element", () => {
    const result = [1, 2, 3].values().reduce((acc, x) => acc + x);
    expect(result).toBe(6);
  });

  test("reduce single element with no initial", () => {
    const result = [42].values().reduce((acc, x) => acc + x);
    expect(result).toBe(42);
  });
});

describe("Iterator.prototype.toArray()", () => {
  test("converts iterator to array", () => {
    const iter = [1, 2, 3].values();
    expect(iter.toArray()).toEqual([1, 2, 3]);
  });

  test("empty iterator to array", () => {
    expect([].values().toArray()).toEqual([]);
  });
});

describe("Iterator.prototype.some()", () => {
  test("returns true if any element matches", () => {
    expect([1, 2, 3].values().some((x) => x === 2)).toBe(true);
  });

  test("returns false if none match", () => {
    expect([1, 2, 3].values().some((x) => x === 5)).toBe(false);
  });
});

describe("Iterator.prototype.every()", () => {
  test("returns true if all elements match", () => {
    expect([2, 4, 6].values().every((x) => x % 2 === 0)).toBe(true);
  });

  test("returns false if any element fails", () => {
    expect([2, 3, 6].values().every((x) => x % 2 === 0)).toBe(false);
  });
});

describe("Iterator.prototype.find()", () => {
  test("returns first matching element", () => {
    expect([1, 2, 3, 4].values().find((x) => x > 2)).toBe(3);
  });

  test("returns undefined if not found", () => {
    expect([1, 2, 3].values().find((x) => x > 10)).toBe(undefined);
  });
});

describe("Iterator.prototype.flatMap()", () => {
  test("maps and flattens arrays", () => {
    const result = [1, 2, 3].values().flatMap((x) => [x, x * 2]).toArray();
    expect(result).toEqual([1, 2, 2, 4, 3, 6]);
  });

  test("non-iterable return value throws TypeError", () => {
    expect(() => {
      [1, 2, 3].values().flatMap((x) => x * 10).toArray();
    }).toThrow();
  });

  test("flatMap is lazy - returns an iterator", () => {
    const fm = [1, 2].values().flatMap((x) => [x, x * 10]);
    expect(fm.next().value).toBe(1);
    expect(fm.next().value).toBe(10);
    expect(fm.next().value).toBe(2);
    expect(fm.next().value).toBe(20);
    expect(fm.next().done).toBe(true);
  });
});

describe("Iterator method chaining", () => {
  test("filter then map", () => {
    const result = [1, 2, 3, 4, 5].values()
      .filter((x) => x % 2 === 1)
      .map((x) => x * 10)
      .toArray();
    expect(result).toEqual([10, 30, 50]);
  });

  test("map then filter then toArray", () => {
    const result = [1, 2, 3, 4].values()
      .map((x) => x * 2)
      .filter((x) => x > 4)
      .toArray();
    expect(result).toEqual([6, 8]);
  });

  test("take then reduce", () => {
    const result = [1, 2, 3, 4, 5].values()
      .take(3)
      .reduce((acc, x) => acc + x, 0);
    expect(result).toBe(6);
  });

  test("drop then some", () => {
    expect([1, 2, 3, 4, 5].values().drop(3).some((x) => x === 5)).toBe(true);
  });
});
