/*---
description: Iterator.prototype.filter helper behavior
features: [Iterator.prototype.filter]
---*/

describe("Iterator.prototype.filter()", () => {
  test("filters values by predicate", () => {
    const result = [1, 2, 3, 4, 5].values().filter((x) => x > 3).toArray();
    expect(result).toEqual([4, 5]);
  });

  test("filter with no matches returns an empty iterator", () => {
    expect([1, 2, 3].values().filter((x) => x > 10).toArray()).toEqual([]);
  });

  test("filter callback receives the iterator index", () => {
    const indices = [];
    [10, 20, 30].values().filter((value, index) => {
      indices.push(index);
      return true;
    }).toArray();

    expect(indices).toEqual([0, 1, 2]);
  });

  test("filter requires a callable callback", () => {
    expect(() => [1].values().filter(null)).toThrow(TypeError);
  });

  test("filter closes the source iterator when the callback throws", () => {
    let closed = 0;
    const source = Iterator.from({
      next() {
        return { value: 1, done: false };
      },
      return() {
        closed = closed + 1;
        return { value: undefined, done: true };
      },
    });

    expect(() => source.filter(() => {
      throw new Error("boom");
    }).next()).toThrow(Error);
    expect(closed).toBe(1);
  });

  test("filter does not consume the source until next() is called", () => {
    const source = [1, 2, 3, 4, 5, 6].values();
    const filtered = source.filter((x) => x % 2 === 0);

    expect(filtered.next().value).toBe(2);
    expect(filtered.next().value).toBe(4);
    expect(filtered.next().value).toBe(6);
    expect(filtered.next().done).toBe(true);
  });

  test("Iterator.from can filter nested iterator groups", () => {
    const makeNext = (arr) => {
      let i = 0;
      return {
        next() {
          if (i < arr.length) {
            const value = arr[i];
            i = i + 1;
            return { value, done: false };
          }
          return { value: undefined, done: true };
        },
      };
    };

    const groups = [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]];
    const outer = Iterator.from(makeNext(groups));
    const result = outer.map((group) =>
      Iterator.from(makeNext(group)).filter((x) => x % 3 === 0).toArray(),
    ).toArray();

    expect(result).toEqual([[3], [6], [9, 12]]);
  });
});
