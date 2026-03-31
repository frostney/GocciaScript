/*---
description: Array.fromAsync creates arrays from async and sync iterables
features: [Array.fromAsync, async-await]
---*/

describe("Array.fromAsync", () => {
  test("returns a Promise", () => {
    const result = Array.fromAsync([1, 2, 3]);
    expect(result instanceof Promise).toBe(true);
  });

  test("rejects when called with no arguments", async () => {
    await expect(Array.fromAsync()).rejects.toThrow(TypeError);
  });

  test("rejects null input", async () => {
    await expect(Array.fromAsync(null)).rejects.toThrow(TypeError);
  });

  test("rejects undefined input", async () => {
    await expect(Array.fromAsync(undefined)).rejects.toThrow(TypeError);
  });

  test("creates arrays from sync arrays", async () => {
    expect(await Array.fromAsync([1, 2, 3])).toEqual([1, 2, 3]);
  });

  test("creates arrays from strings", async () => {
    expect(await Array.fromAsync("abc")).toEqual(["a", "b", "c"]);
  });

  test("creates arrays from Sets", async () => {
    expect(await Array.fromAsync(new Set([10, 20, 30]))).toEqual([10, 20, 30]);
  });

  test("creates arrays from Maps", async () => {
    expect(await Array.fromAsync(new Map([["a", 1], ["b", 2]]))).toEqual([["a", 1], ["b", 2]]);
  });

  test("awaits Promise elements from sync iterables", async () => {
    expect(await Array.fromAsync([
      Promise.resolve("a"),
      Promise.resolve("b"),
      Promise.resolve("c")
    ])).toEqual(["a", "b", "c"]);
  });

  test("creates arrays from async iterables", async () => {
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

    expect(await Array.fromAsync(asyncIterable)).toEqual([10, 20, 30]);
  });

  test("prefers Symbol.asyncIterator over Symbol.iterator", async () => {
    const prioritised = {
      [Symbol.asyncIterator]() {
        return {
          count: 0,
          next() {
            this.count = this.count + 1;
            if (this.count <= 1) {
              return Promise.resolve({ value: "async", done: false });
            }
            return Promise.resolve({ value: undefined, done: true });
          }
        };
      },
      [Symbol.iterator]() {
        return {
          next() {
            return { value: "sync", done: true };
          }
        };
      }
    };

    expect(await Array.fromAsync(prioritised)).toEqual(["async"]);
  });

  test("falls back to array-like input when no iterator is present", async () => {
    const arrayLike = {
      0: Promise.resolve("a"),
      1: "b",
      length: 2,
    };

    expect(await Array.fromAsync(arrayLike)).toEqual(["a", "b"]);
  });

  test("passes thisArg to the mapping function for array-like input", async () => {
    const mappedArrayLike = {
      0: 2,
      1: 4,
      length: 2,
    };

    const thisArg = { factor: 3 };
    const mapper = {
      map(value, index) {
        return value * this.factor + index;
      },
    };

    expect(await Array.fromAsync(mappedArrayLike, mapper.map, thisArg)).toEqual([6, 13]);
  });

  test("rejects when an array-like length getter throws", async () => {
    const value = {
      get length() {
        throw new RangeError("bad length");
      },
    };

    await expect(Array.fromAsync(value)).rejects.toThrow(RangeError);
  });

  test("applies sync mapping functions", async () => {
    const syncMapped = await Array.fromAsync([1, 2, 3], (x, idx) => x * 2 + idx);

    expect(syncMapped).toEqual([2, 5, 8]);
  });

  test("applies async mapping functions", async () => {
    const asyncMapped = await Array.fromAsync([1, 2, 3], async (x) => {
      return await Promise.resolve(x * 10);
    });

    expect(asyncMapped).toEqual([10, 20, 30]);
  });

  test("rejects non-callable mapfn", async () => {
    await expect(Array.fromAsync([1, 2, 3], "not a function")).rejects.toThrow(TypeError);
  });

  test("rejects when a sync iterable element Promise rejects", async () => {
    await expect(Array.fromAsync([
      Promise.resolve(1),
      Promise.reject("element error"),
      Promise.resolve(3)
    ])).rejects.toBe("element error");
  });

  test("rejects when an async iterator next() call rejects", async () => {
    const asyncIterable = {
      [Symbol.asyncIterator]() {
        let i = 0;
        return {
          next() {
            i = i + 1;
            if (i === 2) {
              return Promise.reject("iteration error");
            }
            if (i <= 3) {
              return Promise.resolve({ value: i, done: false });
            }
            return Promise.resolve({ value: undefined, done: true });
          }
        };
      }
    };

    await expect(Array.fromAsync(asyncIterable)).rejects.toBe("iteration error");
  });

  test("closes sync iterators on abrupt completion", async () => {
    let syncClosed = 0;
    const syncIterable = {
      [Symbol.iterator]() {
        let i = 0;
        return {
          next() {
            i = i + 1;
            if (i <= 3) {
              return { value: i, done: false };
            }
            return { value: undefined, done: true };
          },
          return() {
            syncClosed = syncClosed + 1;
            return { value: undefined, done: true };
          },
        };
      },
    };

    await expect(Array.fromAsync(syncIterable, (value) => {
      if (value === 2) {
        throw new Error("boom");
      }
      return value;
    })).rejects.toThrow(Error);

    expect(syncClosed).toBe(1);
  });

  test("closes async iterators on abrupt completion", async () => {
    let asyncClosed = 0;
    const asyncIterable = {
      [Symbol.asyncIterator]() {
        let i = 0;
        return {
          next() {
            i = i + 1;
            if (i <= 3) {
              return Promise.resolve({ value: i, done: false });
            }
            return Promise.resolve({ value: undefined, done: true });
          },
          return() {
            asyncClosed = asyncClosed + 1;
            return Promise.resolve({ value: undefined, done: true });
          },
        };
      },
    };

    await expect(Array.fromAsync(asyncIterable, (value) => {
      if (value === 2) {
        throw new Error("boom");
      }
      return value;
    })).rejects.toThrow(Error);

    expect(asyncClosed).toBe(1);
  });
});
