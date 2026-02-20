/*---
description: Iterator helpers are lazy - they don't eagerly consume the source
features: [Iterator, Iterator.prototype.map, Iterator.prototype.filter, Iterator.prototype.take, Iterator.prototype.drop, Iterator.prototype.flatMap]
---*/

describe("Lazy evaluation", () => {
  test("map does not consume source until next() is called", () => {
    const source = [1, 2, 3, 4, 5].values();
    const mapped = source.map((x) => x * 10);

    expect(mapped.next().value).toBe(10);
    expect(mapped.next().value).toBe(20);

    expect(mapped.toArray()).toEqual([30, 40, 50]);
  });

  test("filter does not consume source until next() is called", () => {
    const source = [1, 2, 3, 4, 5, 6].values();
    const filtered = source.filter((x) => x % 2 === 0);

    expect(filtered.next().value).toBe(2);
    expect(filtered.next().value).toBe(4);
    expect(filtered.next().value).toBe(6);
    expect(filtered.next().done).toBe(true);
  });

  test("take(2) only advances source twice", () => {
    const source = [10, 20, 30, 40, 50].values();
    const taken = source.take(2);

    expect(taken.next().value).toBe(10);
    expect(taken.next().value).toBe(20);
    expect(taken.next().done).toBe(true);

    expect(source.next().value).toBe(30);
  });

  test("drop(2) skips lazily on first next()", () => {
    const source = [1, 2, 3, 4, 5].values();
    const dropped = source.drop(2);

    expect(dropped.next().value).toBe(3);
    expect(dropped.next().value).toBe(4);
    expect(dropped.next().value).toBe(5);
    expect(dropped.next().done).toBe(true);
  });

  test("flatMap is lazy", () => {
    const source = [1, 2, 3].values();
    const fm = source.flatMap((x) => [x, -x]);

    expect(fm.next().value).toBe(1);
    expect(fm.next().value).toBe(-1);
    expect(fm.next().value).toBe(2);
    expect(fm.next().value).toBe(-2);
    expect(fm.next().value).toBe(3);
    expect(fm.next().value).toBe(-3);
    expect(fm.next().done).toBe(true);
  });

  test("chained lazy operations compose correctly", () => {
    const result = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10].values()
      .filter((x) => x % 2 === 0)
      .map((x) => x * 3)
      .take(3)
      .toArray();
    expect(result).toEqual([6, 12, 18]);
  });

  test("drop then take", () => {
    const result = [1, 2, 3, 4, 5, 6, 7].values()
      .drop(2)
      .take(3)
      .toArray();
    expect(result).toEqual([3, 4, 5]);
  });

  test("map preserves laziness through chaining", () => {
    const iter = [1, 2, 3].values()
      .map((x) => x + 10)
      .map((x) => x * 2);

    expect(iter.next().value).toBe(22);
    expect(iter.next().value).toBe(24);
    expect(iter.next().value).toBe(26);
    expect(iter.next().done).toBe(true);
  });

  test("flatMap with empty inner iterables", () => {
    const result = [1, 2, 3].values()
      .flatMap((x) => {
        if (x === 2) {
          return [];
        }
        return [x];
      })
      .toArray();
    expect(result).toEqual([1, 3]);
  });

  test("filter then find short-circuits", () => {
    const result = [1, 2, 3, 4, 5].values()
      .filter((x) => x > 2)
      .find((x) => x === 4);
    expect(result).toBe(4);
  });
});
