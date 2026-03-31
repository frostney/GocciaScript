/*---
description: Iterator.prototype.take helper behavior
features: [Iterator.prototype.take]
---*/

describe("Iterator.prototype.take()", () => {
  test("takes first N elements", () => {
    expect([1, 2, 3, 4, 5].values().take(3).toArray()).toEqual([1, 2, 3]);
  });

  test("take zero returns an empty iterator", () => {
    expect([1, 2, 3].values().take(0).toArray()).toEqual([]);
  });

  test("take more than available returns all elements", () => {
    expect([1, 2].values().take(5).toArray()).toEqual([1, 2]);
  });

  test("take rejects negative counts", () => {
    expect(() => [1].values().take(-1)).toThrow(RangeError);
  });

  test("take only advances the source as needed", () => {
    const source = [10, 20, 30, 40, 50].values();
    const taken = source.take(2);

    expect(taken.next().value).toBe(10);
    expect(taken.next().value).toBe(20);
    expect(taken.next().done).toBe(true);
    expect(source.next().value).toBe(30);
  });

  test("take composes after drop", () => {
    const result = [1, 2, 3, 4, 5, 6, 7].values()
      .drop(2)
      .take(3)
      .toArray();

    expect(result).toEqual([3, 4, 5]);
  });

  test("take can bound nested iterators inside helper chains", () => {
    const data = [5, 2, 8, 1, 9];
    const result = Iterator.from(data[Symbol.iterator]())
      .filter((n) => n > 3)
      .map((n) =>
        Iterator.from(
          Array.from({ length: n }, (_, i) => i + 1)[Symbol.iterator](),
        ).take(3).toArray(),
      )
      .toArray();

    expect(result).toEqual([[1, 2, 3], [1, 2, 3], [1, 2, 3]]);
  });
});
