/*---
description: Iterator.prototype.flatMap helper behavior
features: [Iterator.prototype.flatMap]
---*/

describe("Iterator.prototype.flatMap()", () => {
  test("maps and flattens iterables", () => {
    const result = [1, 2, 3].values().flatMap((x) => [x, x * 2]).toArray();
    expect(result).toEqual([1, 2, 2, 4, 3, 6]);
  });

  test("flatMap callback receives the iterator index", () => {
    const indices = [];
    [10, 20].values().flatMap((value, index) => {
      indices.push(index);
      return [value];
    }).toArray();

    expect(indices).toEqual([0, 1]);
  });

  test("flatMap throws when the callback does not return an iterable", () => {
    expect(() => [1].values().flatMap((x) => x * 10).toArray()).toThrow(TypeError);
  });

  test("flatMap closes the source iterator when the callback throws", () => {
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

    expect(() => source.flatMap(() => {
      throw new Error("boom");
    }).next()).toThrow(Error);
    expect(closed).toBe(1);
  });

  test("flatMap is lazy", () => {
    const source = [1, 2, 3].values();
    const iterator = source.flatMap((x) => [x, -x]);

    expect(iterator.next().value).toBe(1);
    expect(iterator.next().value).toBe(-1);
    expect(iterator.next().value).toBe(2);
    expect(iterator.next().value).toBe(-2);
    expect(iterator.next().value).toBe(3);
    expect(iterator.next().value).toBe(-3);
    expect(iterator.next().done).toBe(true);
  });

  test("flatMap skips empty inner iterables", () => {
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

  test("flatMap can return iterators with nested helper chains", () => {
    const outer = Iterator.from([1, 2, 3][Symbol.iterator]());
    const result = outer
      .flatMap((x) =>
        Iterator.from([10, 20, 30][Symbol.iterator]())
          .filter((y) => (x + y / 10) % 2 === 0),
      )
      .toArray();

    expect(result).toEqual([10, 30, 20, 10, 30]);
  });

  test("flatMap can consume dropped inner iterators", () => {
    const result = Iterator.from([3, 5, 7][Symbol.iterator]())
      .flatMap((n) =>
        Iterator.from(
          Array.from({ length: n }, (_, i) => i)[Symbol.iterator](),
        ).drop(n - 2),
      )
      .toArray();

    expect(result).toEqual([1, 2, 3, 4, 5, 6]);
  });
});
