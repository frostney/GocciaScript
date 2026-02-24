/*---
description: >
  Edge cases for for-await-of loops per ES2026 §14.7.5.6 and test262
  test/language/statements/for-await-of/
features: [async-await, for-of]
---*/

describe("for-await-of with non-iterable values", () => {
  test("null throws TypeError", async () => {
    await expect((async () => {
      for await (const x of null) {}
    })()).rejects.toThrow(TypeError);
  });

  test("undefined throws TypeError", async () => {
    await expect((async () => {
      for await (const x of undefined) {}
    })()).rejects.toThrow(TypeError);
  });

  test("number throws TypeError", async () => {
    await expect((async () => {
      for await (const x of 42) {}
    })()).rejects.toThrow(TypeError);
  });

  test("boolean throws TypeError", async () => {
    await expect((async () => {
      for await (const x of true) {}
    })()).rejects.toThrow(TypeError);
  });

  test("plain object without iterator throws TypeError", async () => {
    await expect((async () => {
      for await (const x of { a: 1 }) {}
    })()).rejects.toThrow(TypeError);
  });
});

describe("for-await-of with rejected promises in sync iterable", () => {
  test("rejected promise value throws in loop body", async () => {
    const values = [
      Promise.resolve(1),
      Promise.reject("fail"),
      Promise.resolve(3)
    ];
    const results = [];
    try {
      for await (const v of values) {
        results.push(v);
      }
    } catch (e) {
      expect(e).toBe("fail");
      expect(results).toEqual([1]);
      return;
    }
    expect(true).toBe(false);
  });

  test("all resolved promises are collected", async () => {
    const values = [
      Promise.resolve("a"),
      Promise.resolve("b"),
      Promise.resolve("c")
    ];
    const results = [];
    for await (const v of values) {
      results.push(v);
    }
    expect(results).toEqual(["a", "b", "c"]);
  });
});

describe("for-await-of with async iterables", () => {
  test("async iterable with all resolved next() results", async () => {
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
          }
        };
      }
    };
    const results = [];
    for await (const v of asyncIterable) {
      results.push(v);
    }
    expect(results).toEqual([1, 2, 3]);
  });

  test("async iterable with rejected next() throws", async () => {
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
    const results = [];
    try {
      for await (const v of asyncIterable) {
        results.push(v);
      }
    } catch (e) {
      expect(e).toBe("iteration error");
      expect(results).toEqual([1]);
      return;
    }
    expect(true).toBe(false);
  });

  test("async iterable with immediate done", async () => {
    const asyncIterable = {
      [Symbol.asyncIterator]() {
        return {
          next() {
            return Promise.resolve({ value: undefined, done: true });
          }
        };
      }
    };
    const results = [];
    for await (const v of asyncIterable) {
      results.push(v);
    }
    expect(results).toEqual([]);
  });

  test("async iterable with non-Promise next() return", async () => {
    const asyncIterable = {
      [Symbol.asyncIterator]() {
        let i = 0;
        return {
          next() {
            i = i + 1;
            if (i <= 2) {
              return { value: i * 10, done: false };
            }
            return { value: undefined, done: true };
          }
        };
      }
    };
    const results = [];
    for await (const v of asyncIterable) {
      results.push(v);
    }
    expect(results).toEqual([10, 20]);
  });
});

describe("for-await-of falls back to sync iterable", () => {
  test("uses Symbol.iterator when no Symbol.asyncIterator", async () => {
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
          }
        };
      }
    };
    const results = [];
    for await (const v of syncIterable) {
      results.push(v);
    }
    expect(results).toEqual([1, 2, 3]);
  });

  test("awaits Promise values from sync iterator", async () => {
    const results = [];
    for await (const v of [Promise.resolve(10), Promise.resolve(20)]) {
      results.push(v);
    }
    expect(results).toEqual([10, 20]);
  });

  test("non-Promise values from sync iterator pass through", async () => {
    const results = [];
    for await (const v of [1, "two", true]) {
      results.push(v);
    }
    expect(results).toEqual([1, "two", true]);
  });
});

describe("for-await-of with break", () => {
  test("break exits async iterable loop early", async () => {
    const asyncIterable = {
      [Symbol.asyncIterator]() {
        let i = 0;
        return {
          next() {
            i = i + 1;
            if (i <= 100) {
              return Promise.resolve({ value: i, done: false });
            }
            return Promise.resolve({ value: undefined, done: true });
          }
        };
      }
    };
    const results = [];
    for await (const v of asyncIterable) {
      results.push(v);
      if (v === 3) {
        break;
      }
    }
    expect(results).toEqual([1, 2, 3]);
  });

  test("break exits sync fallback loop early", async () => {
    const results = [];
    for await (const v of [10, 20, 30, 40, 50]) {
      results.push(v);
      if (v === 30) {
        break;
      }
    }
    expect(results).toEqual([10, 20, 30]);
  });
});

describe("for-await-of with destructuring", () => {
  test("array destructuring from async iterable", async () => {
    const asyncIterable = {
      [Symbol.asyncIterator]() {
        const pairs = [[1, "a"], [2, "b"], [3, "c"]];
        let i = 0;
        return {
          next() {
            if (i < pairs.length) {
              const value = pairs[i];
              i = i + 1;
              return Promise.resolve({ value, done: false });
            }
            return Promise.resolve({ value: undefined, done: true });
          }
        };
      }
    };
    const nums = [];
    const chars = [];
    for await (const [n, c] of asyncIterable) {
      nums.push(n);
      chars.push(c);
    }
    expect(nums).toEqual([1, 2, 3]);
    expect(chars).toEqual(["a", "b", "c"]);
  });

  test("object destructuring from async iterable", async () => {
    const asyncIterable = {
      [Symbol.asyncIterator]() {
        const items = [{ name: "Alice", age: 30 }, { name: "Bob", age: 25 }];
        let i = 0;
        return {
          next() {
            if (i < items.length) {
              const value = items[i];
              i = i + 1;
              return Promise.resolve({ value, done: false });
            }
            return Promise.resolve({ value: undefined, done: true });
          }
        };
      }
    };
    const names = [];
    for await (const { name } of asyncIterable) {
      names.push(name);
    }
    expect(names).toEqual(["Alice", "Bob"]);
  });
});

describe("for-await-of with Map and Set", () => {
  test("for-await-of with Map (sync fallback)", async () => {
    const map = new Map();
    map.set("a", 1);
    map.set("b", 2);
    const entries = [];
    for await (const [k, v] of map) {
      entries.push([k, v]);
    }
    expect(entries).toEqual([["a", 1], ["b", 2]]);
  });

  test("for-await-of with Set (sync fallback)", async () => {
    const set = new Set([10, 20, 30]);
    const results = [];
    for await (const v of set) {
      results.push(v);
    }
    expect(results).toEqual([10, 20, 30]);
  });
});

describe("for-await-of with mixed Promise/non-Promise values", () => {
  test("mix of Promise and plain values from sync iterable", async () => {
    const results = [];
    for await (const v of [Promise.resolve(1), 2, Promise.resolve(3), 4]) {
      results.push(v);
    }
    expect(results).toEqual([1, 2, 3, 4]);
  });

  test("null and undefined Promise resolutions", async () => {
    const results = [];
    for await (const v of [Promise.resolve(null), Promise.resolve(undefined)]) {
      results.push(v);
    }
    expect(results[0]).toBeNull();
    expect(results[1]).toBeUndefined();
  });
});
