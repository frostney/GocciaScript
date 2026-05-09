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
    // Source is closed when the take limit is reached (IteratorClose per spec)
    expect(source.next().done).toBe(true);
  });

  test("take composes after drop", () => {
    const result = [1, 2, 3, 4, 5, 6, 7].values()
      .drop(2)
      .take(3)
      .toArray();

    expect(result).toEqual([3, 4, 5]);
  });

  test("take closes source iterator when limit is reached", () => {
    let closed = false;
    const source = {
      [Symbol.iterator]() {
        let i = 0;
        return {
          next() {
            i++;
            return { value: i, done: false };
          },
          return() {
            closed = true;
            return { value: undefined, done: true };
          },
        };
      },
    };

    const result = Iterator.from(source[Symbol.iterator]()).take(2).toArray();
    expect(result).toEqual([1, 2]);
    expect(closed).toBe(true);
  });

  test("take runs generator finally blocks when limit is reached", () => {
    let finalized = false;
    const gen = {
      *go() {
        try { yield 1; yield 2; yield 3; }
        finally { finalized = true; }
      },
    }.go;

    const arr = gen().take(1).toArray();
    expect(arr).toEqual([1]);
    expect(finalized).toBe(true);
  });

  test("take does not close source when source is exhausted before limit", () => {
    let closed = false;
    const source = {
      [Symbol.iterator]() {
        let i = 0;
        return {
          next() {
            i++;
            if (i > 2) return { value: undefined, done: true };
            return { value: i, done: false };
          },
          return() {
            closed = true;
            return { value: undefined, done: true };
          },
        };
      },
    };

    const result = Iterator.from(source[Symbol.iterator]()).take(5).toArray();
    expect(result).toEqual([1, 2]);
    expect(closed).toBe(false);
  });

  test("take(0) closes source iterator immediately", () => {
    let closed = false;
    const source = {
      [Symbol.iterator]() {
        return {
          next() { return { value: 1, done: false }; },
          return() {
            closed = true;
            return { value: undefined, done: true };
          },
        };
      },
    };

    const result = Iterator.from(source[Symbol.iterator]()).take(0).toArray();
    expect(result).toEqual([]);
    expect(closed).toBe(true);
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
