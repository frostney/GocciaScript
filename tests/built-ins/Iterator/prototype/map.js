/*---
description: Iterator.prototype.map helper behavior
features: [Iterator.prototype.map]
---*/

describe("Iterator.prototype.map()", () => {
  test("maps values through a callback", () => {
    const result = [1, 2, 3].values().map((x) => x * 2).toArray();
    expect(result).toEqual([2, 4, 6]);
  });

  test("map is lazy", () => {
    const mapped = [1, 2, 3].values().map((x) => x * 2);
    expect(mapped.next().value).toBe(2);
    expect(mapped.next().value).toBe(4);
    expect(mapped.next().value).toBe(6);
    expect(mapped.next().done).toBe(true);
  });

  test("map callback receives the iterator index", () => {
    const indices = [];
    [10, 20, 30].values().map((value, index) => {
      indices.push(index);
      return value;
    }).toArray();

    expect(indices).toEqual([0, 1, 2]);
  });

  test("map requires a callable callback", () => {
    expect(() => [1].values().map(42)).toThrow(TypeError);
  });

  test("map closes the source iterator when the callback throws", () => {
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

    expect(() => source.map(() => {
      throw new Error("boom");
    }).next()).toThrow(Error);
    expect(closed).toBe(1);
  });

  test("map does not consume the source until next() is called", () => {
    const source = [1, 2, 3, 4, 5].values();
    const mapped = source.map((x) => x * 10);

    expect(mapped.next().value).toBe(10);
    expect(mapped.next().value).toBe(20);
    expect(mapped.toArray()).toEqual([30, 40, 50]);
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

  test("Iterator.from inside Iterator.from().map", () => {
    const makeNext = (max) => {
      let i = 0;
      return {
        next() {
          i = i + 1;
          if (i <= max) {
            return { value: i, done: false };
          }
          return { value: undefined, done: true };
        },
      };
    };

    const outer = Iterator.from(makeNext(3));
    const result = outer.map((n) => Iterator.from(makeNext(n)).toArray()).toArray();
    expect(result).toEqual([[1], [1, 2], [1, 2, 3]]);
  });

  test("map can consume nested helper chains", () => {
    const data = [1, 2, 3];
    const result = Iterator.from(data[Symbol.iterator]())
      .map((x) =>
        Iterator.from(data[Symbol.iterator]())
          .map((y) => x * y)
          .filter((p) => p % 2 === 0)
          .toArray(),
      )
      .toArray();

    expect(result).toEqual([[2], [2, 4, 6], [6]]);
  });

  test("outer iterator state is preserved while map consumes inner iterators", () => {
    let outerCalls = 0;
    const outerIter = {
      next() {
        outerCalls = outerCalls + 1;
        if (outerCalls <= 3) {
          return { value: outerCalls * 10, done: false };
        }
        return { value: undefined, done: true };
      },
    };

    const result = Iterator.from(outerIter).map((outerValue) => {
      let innerCalls = 0;
      const innerIter = {
        next() {
          innerCalls = innerCalls + 1;
          if (innerCalls <= 2) {
            return { value: outerValue + innerCalls, done: false };
          }
          return { value: undefined, done: true };
        },
      };
      return Iterator.from(innerIter).toArray();
    }).toArray();

    expect(result).toEqual([[11, 12], [21, 22], [31, 32]]);
    expect(outerCalls).toBe(4);
  });

  test("map can create multiple inner iterators from the same factory", () => {
    const makeIter = (start) => {
      let i = start;
      return {
        next() {
          if (i < start + 3) {
            const value = i;
            i = i + 1;
            return { value, done: false };
          }
          return { value: undefined, done: true };
        },
      };
    };

    const result = Iterator.from(makeIter(0))
      .map((x) => Iterator.from(makeIter(x * 10)).reduce((a, b) => a + b, 0))
      .toArray();

    expect(result).toEqual([3, 33, 63]);
  });
});
