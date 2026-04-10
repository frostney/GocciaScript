/*---
description: Iterator.concat() concatenates multiple iterables into a single iterator
features: [Iterator, Iterator.concat]
---*/

describe("Iterator.concat()", () => {
  test("concatenates two arrays", () => {
    const result = Iterator.concat([1, 2], [3, 4]).toArray();
    expect(result).toEqual([1, 2, 3, 4]);
  });

  test("concatenates three arrays", () => {
    const result = Iterator.concat([1], [2, 3], [4, 5, 6]).toArray();
    expect(result).toEqual([1, 2, 3, 4, 5, 6]);
  });

  test("concatenates zero arguments (empty)", () => {
    const result = Iterator.concat().toArray();
    expect(result).toEqual([]);
  });

  test("concatenates a single array", () => {
    const result = Iterator.concat([10, 20, 30]).toArray();
    expect(result).toEqual([10, 20, 30]);
  });

  test("concatenates empty arrays", () => {
    const result = Iterator.concat([], [1, 2], [], [3], []).toArray();
    expect(result).toEqual([1, 2, 3]);
  });

  test("concatenates Sets", () => {
    const a = new Set([1, 2]);
    const b = new Set([3, 4]);
    const result = Iterator.concat(a, b).toArray();
    expect(result).toEqual([1, 2, 3, 4]);
  });

  test("concatenates Maps (yields entries)", () => {
    const a = new Map([["x", 1]]);
    const b = new Map([["y", 2]]);
    const result = Iterator.concat(a, b).toArray();
    expect(result[0][0]).toBe("x");
    expect(result[0][1]).toBe(1);
    expect(result[1][0]).toBe("y");
    expect(result[1][1]).toBe(2);
  });

  test("concatenates mixed iterable types", () => {
    const arr = [1, 2];
    const set = new Set([3, 4]);
    const result = Iterator.concat(arr, set).toArray();
    expect(result).toEqual([1, 2, 3, 4]);
  });

  test("concatenates strings (character iteration)", () => {
    const result = Iterator.concat("ab", "cd").toArray();
    expect(result).toEqual(["a", "b", "c", "d"]);
  });

  test("concatenates string with array", () => {
    const result = Iterator.concat("hi", [1, 2]).toArray();
    expect(result).toEqual(["h", "i", 1, 2]);
  });

  test("is lazy — does not consume iterables until next() is called", () => {
    let opened = 0;
    const iterable1 = {
      [Symbol.iterator]() {
        opened = opened + 1;
        return [1, 2].values();
      },
    };
    const iterable2 = {
      [Symbol.iterator]() {
        opened = opened + 1;
        return [3, 4].values();
      },
    };

    const iter = Iterator.concat(iterable1, iterable2);
    expect(opened).toBe(0);

    expect(iter.next().value).toBe(1);
    expect(opened).toBe(1);

    expect(iter.next().value).toBe(2);
    expect(opened).toBe(1);

    expect(iter.next().value).toBe(3);
    expect(opened).toBe(2);

    expect(iter.next().value).toBe(4);
    expect(opened).toBe(2);

    expect(iter.next().done).toBe(true);
  });

  test("returns done after exhaustion", () => {
    const iter = Iterator.concat([1]);
    expect(iter.next()).toEqual({ value: 1, done: false });
    expect(iter.next()).toEqual({ value: undefined, done: true });
    expect(iter.next()).toEqual({ value: undefined, done: true });
  });

  test("result is an iterator (has next and Symbol.iterator)", () => {
    const iter = Iterator.concat([1]);
    expect(typeof iter.next).toBe("function");
    expect(iter[Symbol.iterator]()).toBe(iter);
  });

  test("result works with iterator helper methods", () => {
    const result = Iterator.concat([1, 2, 3], [4, 5, 6])
      .filter((x) => x % 2 === 0)
      .map((x) => x * 10)
      .toArray();
    expect(result).toEqual([20, 40, 60]);
  });

  test("validates all arguments upfront before iterating", () => {
    expect(() => Iterator.concat([1], 42, [3])).toThrow(TypeError);
    expect(() => Iterator.concat([1], null)).toThrow(TypeError);
    expect(() => Iterator.concat(undefined)).toThrow(TypeError);
    expect(() => Iterator.concat([1], true)).toThrow(TypeError);
  });

  test("throws TypeError for object without Symbol.iterator", () => {
    expect(() => Iterator.concat({ a: 1 })).toThrow(TypeError);
  });

  test("concatenates user-defined iterables", () => {
    const range = (start, end) => ({
      [Symbol.iterator]() {
        let i = start;
        return {
          next() {
            if (i <= end) {
              const v = i;
              i = i + 1;
              return { value: v, done: false };
            }
            return { value: undefined, done: true };
          },
        };
      },
    });

    const result = Iterator.concat(range(1, 3), range(10, 12)).toArray();
    expect(result).toEqual([1, 2, 3, 10, 11, 12]);
  });

  test("closes inner iterator on early termination via take", () => {
    const result = Iterator.concat([1, 2, 3], [4, 5, 6]).take(4).toArray();
    expect(result).toEqual([1, 2, 3, 4]);
  });

  test("works with for-of", () => {
    const result = [];
    const iter = Iterator.concat([1, 2], [3]);
    for (const v of iter) {
      result.push(v);
    }
    expect(result).toEqual([1, 2, 3]);
  });

  test("handles large number of iterables", () => {
    const arrays = Array.from({ length: 100 }, (_, i) => [i]);
    const expected = Array.from({ length: 100 }, (_, i) => i);
    const result = Iterator.concat(...arrays).toArray();
    expect(result).toEqual(expected);
  });
});
