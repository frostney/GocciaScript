/*---
description: Array.fromAsync creates arrays from async and sync iterables
features: [Array.fromAsync, async-await]
---*/

describe("Array.fromAsync", () => {
  test("creates array from async iterable", () => {
    const asyncIterable = {
      [Symbol.asyncIterator]() {
        let i = 0;
        return {
          next() {
            i = i + 1;
            if (i <= 3) {
              return Promise.resolve({ value: i * 10, done: false });
            }
            return Promise.resolve({ value: undefined, done: true });
          }
        };
      }
    };

    return Array.fromAsync(asyncIterable).then((result) => {
      expect(result).toEqual([10, 20, 30]);
    });
  });

  test("creates array from sync iterable", () => {
    return Array.fromAsync([1, 2, 3]).then((result) => {
      expect(result).toEqual([1, 2, 3]);
    });
  });

  test("awaits Promise elements from sync iterable", () => {
    return Array.fromAsync([
      Promise.resolve("a"),
      Promise.resolve("b"),
      Promise.resolve("c")
    ]).then((result) => {
      expect(result).toEqual(["a", "b", "c"]);
    });
  });

  test("returns a Promise", () => {
    const result = Array.fromAsync([1, 2, 3]);
    expect(result instanceof Promise).toBe(true);
  });

  test("with mapping function", () => {
    return Array.fromAsync([1, 2, 3], (x) => x * 2).then((result) => {
      expect(result).toEqual([2, 4, 6]);
    });
  });

  test("empty iterable returns empty array", () => {
    return Array.fromAsync([]).then((result) => {
      expect(result).toEqual([]);
    });
  });
});
