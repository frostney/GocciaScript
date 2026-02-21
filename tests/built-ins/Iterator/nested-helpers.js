/*---
description: Nested iterator protocol and lazy helper chains
features: [Iterator, Iterator.from, Iterator.prototype.map, Iterator.prototype.filter, Iterator.prototype.take, Iterator.prototype.drop, Iterator.prototype.flatMap, Symbol.iterator]
---*/

describe("nested user-defined iterables", () => {
  test("spread inside iterable's next() callback", () => {
    const makeRange = (start, end) => ({
      [Symbol.iterator]() {
        let i = start;
        return {
          next() {
            if (i <= end) {
              const val = i;
              i = i + 1;
              return { value: val, done: false };
            }
            return { value: undefined, done: true };
          }
        };
      }
    });

    const outer = makeRange(1, 3);
    const result = Array.from(outer, (n) => [...makeRange(1, n)]);
    expect(result).toEqual([[1], [1, 2], [1, 2, 3]]);
  });

  test("nested iterables consumed via destructuring", () => {
    const pairs = {
      [Symbol.iterator]() {
        let i = 0;
        const data = [[10, 20], [30, 40]];
        return {
          next() {
            if (i < data.length) {
              const val = data[i];
              i = i + 1;
              return { value: val, done: false };
            }
            return { value: undefined, done: true };
          }
        };
      }
    };

    const collected = [];
    Array.from(pairs).forEach(([a, b]) => {
      collected.push(a + b);
    });
    expect(collected).toEqual([30, 70]);
  });

  test("iterable producing iterables consumed with Array.from", () => {
    const makeCounter = (n) => ({
      [Symbol.iterator]() {
        let i = 0;
        return {
          next() {
            if (i < n) {
              i = i + 1;
              return { value: i, done: false };
            }
            return { value: undefined, done: true };
          }
        };
      }
    });

    const outer = makeCounter(3);
    const result = Array.from(outer, (n) => Array.from(makeCounter(n)));
    expect(result).toEqual([[1], [1, 2], [1, 2, 3]]);
  });
});

describe("nested Iterator.from with plain {next()} objects", () => {
  test("Iterator.from inside Iterator.from().map", () => {
    const makeNext = (max) => {
      let i = 0;
      return { next() {
        i = i + 1;
        if (i <= max) { return { value: i, done: false }; }
        return { value: undefined, done: true };
      }};
    };

    const outer = Iterator.from(makeNext(3));
    const result = outer.map((n) =>
      Iterator.from(makeNext(n)).toArray()
    ).toArray();
    expect(result).toEqual([[1], [1, 2], [1, 2, 3]]);
  });

  test("Iterator.from nested filter chains", () => {
    const makeNext = (arr) => {
      let i = 0;
      return { next() {
        if (i < arr.length) {
          const val = arr[i];
          i = i + 1;
          return { value: val, done: false };
        }
        return { value: undefined, done: true };
      }};
    };

    const groups = [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]];
    const outer = Iterator.from(makeNext(groups));
    const result = outer.map((g) =>
      Iterator.from(makeNext(g)).filter((x) => x % 3 === 0).toArray()
    ).toArray();
    expect(result).toEqual([[3], [6], [9, 12]]);
  });
});

describe("nested lazy iterator helper chains", () => {
  test("map().filter() inside outer map()", () => {
    const data = [1, 2, 3];
    const result = Iterator.from(data[Symbol.iterator]())
      .map((x) =>
        Iterator.from(data[Symbol.iterator]())
          .map((y) => x * y)
          .filter((p) => p % 2 === 0)
          .toArray()
      )
      .toArray();
    // x=1: [1,2,3] → [1,2,3] → evens: [2]
    // x=2: [2,4,6] → evens: [2,4,6]
    // x=3: [3,6,9] → evens: [6]
    expect(result).toEqual([[2], [2, 4, 6], [6]]);
  });

  test("flatMap producing iterators that themselves are chained", () => {
    const outer = Iterator.from([1, 2, 3][Symbol.iterator]());
    const result = outer
      .flatMap((x) =>
        Iterator.from([10, 20, 30][Symbol.iterator]())
          .filter((y) => (x + y / 10) % 2 === 0)
      )
      .toArray();
    // x=1: y=10→1+1=2✓, y=20→1+2=3✗, y=30→1+3=4✓ → [10,30]
    // x=2: y=10→2+1=3✗, y=20→2+2=4✓, y=30→2+3=5✗ → [20]
    // x=3: y=10→3+1=4✓, y=20→3+2=5✗, y=30→3+3=6✓ → [10,30]
    expect(result).toEqual([10, 30, 20, 10, 30]);
  });

  test("take inside map inside filter", () => {
    const data = [5, 2, 8, 1, 9];
    const result = Iterator.from(data[Symbol.iterator]())
      .filter((n) => n > 3)
      .map((n) =>
        Iterator.from(
          Array.from({ length: n }, (_, i) => i + 1)[Symbol.iterator]()
        ).take(3).toArray()
      )
      .toArray();
    // filter → [5, 8, 9]
    // map(5) → take(3) from [1,2,3,4,5] → [1,2,3]
    // map(8) → take(3) from [1..8] → [1,2,3]
    // map(9) → take(3) from [1..9] → [1,2,3]
    expect(result).toEqual([[1, 2, 3], [1, 2, 3], [1, 2, 3]]);
  });

  test("drop inside flatMap", () => {
    const result = Iterator.from([3, 5, 7][Symbol.iterator]())
      .flatMap((n) =>
        Iterator.from(
          Array.from({ length: n }, (_, i) => i)[Symbol.iterator]()
        ).drop(n - 2)
      )
      .toArray();
    // n=3: [0,1,2] drop(1) → [1,2]
    // n=5: [0,1,2,3,4] drop(3) → [3,4]
    // n=7: [0,1,2,3,4,5,6] drop(5) → [5,6]
    expect(result).toEqual([1, 2, 3, 4, 5, 6]);
  });
});

describe("nested iterators with shared closure state", () => {
  test("outer iterator state not corrupted by inner consumption", () => {
    let outerCalls = 0;
    const outerIter = {
      next() {
        outerCalls = outerCalls + 1;
        if (outerCalls <= 3) {
          return { value: outerCalls * 10, done: false };
        }
        return { value: undefined, done: true };
      }
    };

    const result = Iterator.from(outerIter).map((outerVal) => {
      let innerCalls = 0;
      const innerIter = {
        next() {
          innerCalls = innerCalls + 1;
          if (innerCalls <= 2) {
            return { value: outerVal + innerCalls, done: false };
          }
          return { value: undefined, done: true };
        }
      };
      return Iterator.from(innerIter).toArray();
    }).toArray();

    expect(result).toEqual([[11, 12], [21, 22], [31, 32]]);
    expect(outerCalls).toBe(4);
  });

  test("multiple inner iterators from same factory", () => {
    const makeIter = (start) => {
      let i = start;
      return {
        next() {
          if (i < start + 3) {
            const val = i;
            i = i + 1;
            return { value: val, done: false };
          }
          return { value: undefined, done: true };
        }
      };
    };

    const result = Iterator.from(makeIter(0))
      .map((x) => Iterator.from(makeIter(x * 10)).reduce((a, b) => a + b, 0))
      .toArray();
    // x=0: sum(0,1,2) = 3
    // x=1: sum(10,11,12) = 33
    // x=2: sum(20,21,22) = 63
    expect(result).toEqual([3, 33, 63]);
  });
});

describe("nested iterators with reduce and every/some/find", () => {
  test("reduce consuming inner iterators", () => {
    const result = Iterator.from([1, 2, 3][Symbol.iterator]())
      .reduce((acc, x) => {
        const inner = Iterator.from([x, x * 2, x * 3][Symbol.iterator]())
          .filter((v) => v > 2)
          .toArray();
        return [...acc, ...inner];
      }, []);
    // x=1: [1,2,3] filter>2 → [3]
    // x=2: [2,4,6] filter>2 → [4,6]
    // x=3: [3,6,9] filter>2 → [3,6,9]
    expect(result).toEqual([3, 4, 6, 3, 6, 9]);
  });

  test("some with inner iterator check", () => {
    const data = [[1, 3, 5], [2, 4, 6], [7, 9, 11]];
    const result = Iterator.from(data[Symbol.iterator]())
      .some((group) =>
        Iterator.from(group[Symbol.iterator]())
          .every((x) => x % 2 === 0)
      );
    expect(result).toBe(true);
  });

  test("find with inner iterator computation", () => {
    const data = [[1, 1, 1], [2, 3, 4], [10, 20, 30]];
    const result = Iterator.from(data[Symbol.iterator]())
      .find((group) =>
        Iterator.from(group[Symbol.iterator]())
          .reduce((a, b) => a + b, 0) > 15
      );
    expect(result).toEqual([10, 20, 30]);
  });
});
