/*---
description: User-defined iterables - objects with [Symbol.iterator]() returning {next()} work with spread, destructuring, Array.from
features: [Iterator, Symbol.iterator]
---*/

describe("User-defined iterables", () => {
  test("object with Symbol.iterator works with spread", () => {
    const iterable = {
      [Symbol.iterator]() {
        let i = 0;
        return {
          next() {
            i = i + 1;
            if (i <= 3) {
              return { value: i * 10, done: false };
            }
            return { value: undefined, done: true };
          }
        };
      }
    };
    expect([...iterable]).toEqual([10, 20, 30]);
  });

  test("user-defined iterable works with destructuring", () => {
    const iterable = {
      [Symbol.iterator]() {
        let i = 0;
        const values = ["a", "b", "c"];
        return {
          next() {
            if (i < values.length) {
              const val = values[i];
              i = i + 1;
              return { value: val, done: false };
            }
            return { value: undefined, done: true };
          }
        };
      }
    };
    const [a, b, c] = iterable;
    expect(a).toBe("a");
    expect(b).toBe("b");
    expect(c).toBe("c");
  });

  test("user-defined iterable works with Array.from", () => {
    const iterable = {
      [Symbol.iterator]() {
        let i = 0;
        return {
          next() {
            i = i + 1;
            if (i <= 3) {
              return { value: i, done: false };
            }
            return { value: undefined, done: true };
          }
        };
      }
    };
    expect(Array.from(iterable)).toEqual([1, 2, 3]);
  });

  test("user-defined iterable works with Array.from and map", () => {
    const iterable = {
      [Symbol.iterator]() {
        let i = 0;
        return {
          next() {
            i = i + 1;
            if (i <= 3) {
              return { value: i, done: false };
            }
            return { value: undefined, done: true };
          }
        };
      }
    };
    expect(Array.from(iterable, (x) => x * 2)).toEqual([2, 4, 6]);
  });

  test("iterator with done omitted is treated as false", () => {
    const iterable = {
      [Symbol.iterator]() {
        let called = false;
        return {
          next() {
            if (!called) {
              called = true;
              return { value: 42 };
            }
            return { done: true };
          }
        };
      }
    };
    expect([...iterable]).toEqual([42]);
  });

  test("empty user-defined iterable", () => {
    const iterable = {
      [Symbol.iterator]() {
        return {
          next() {
            return { value: undefined, done: true };
          }
        };
      }
    };
    expect([...iterable]).toEqual([]);
  });
});
