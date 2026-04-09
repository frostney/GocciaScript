/*---
description: Top-level for-await-of outside async functions
features: [for-of, async-await, top-level-await]
---*/

// True top-level for-await-of: runs at module scope, not inside any function
const topLevelResult = [];
for await (const value of [Promise.resolve(1), Promise.resolve(2), Promise.resolve(3)]) {
  topLevelResult.push(value);
}

describe("top-level for-await-of", () => {
  test("for-await-of iterates at file scope", () => {
    expect(topLevelResult).toEqual([1, 2, 3]);
  });

  test("iterates async iterable at top level", async () => {
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
        };
      },
    };

    const result = [];
    for await (const item of asyncIterable) {
      result.push(item);
    }
    expect(result).toEqual([1, 2, 3]);
  });

  test("iterates sync iterable at top level", async () => {
    const result = [];
    for await (const item of [10, 20, 30]) {
      result.push(item);
    }
    expect(result).toEqual([10, 20, 30]);
  });

  test("awaits Promise values from sync iterator at top level", async () => {
    const promises = [
      Promise.resolve(1),
      Promise.resolve(2),
      Promise.resolve(3),
    ];
    const result = [];
    for await (const item of promises) {
      result.push(item);
    }
    expect(result).toEqual([1, 2, 3]);
  });

  test("break works in top-level for-await-of", async () => {
    const asyncIterable = {
      [Symbol.asyncIterator]() {
        let i = 0;
        return {
          next() {
            i = i + 1;
            return Promise.resolve({
              value: i,
              done: i > 10,
            });
          },
        };
      },
    };

    const result = [];
    for await (const item of asyncIterable) {
      if (item === 3) {
        break;
      }
      result.push(item);
    }
    expect(result).toEqual([1, 2]);
  });

  test("destructuring in top-level for-await-of", async () => {
    const asyncIterable = {
      [Symbol.asyncIterator]() {
        const pairs = [
          [1, "a"],
          [2, "b"],
          [3, "c"],
        ];
        let i = 0;
        return {
          next() {
            if (i < pairs.length) {
              const value = pairs[i];
              i = i + 1;
              return Promise.resolve({ value, done: false });
            }
            return Promise.resolve({ value: undefined, done: true });
          },
        };
      },
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

  test("error handling in top-level for-await-of", async () => {
    const values = [
      Promise.resolve(1),
      Promise.reject("fail"),
      Promise.resolve(3),
    ];
    const result = [];

    try {
      for await (const value of values) {
        result.push(value);
      }
    } catch (e) {
      expect(e).toBe("fail");
      expect(result).toEqual([1]);
      return;
    }

    expect(true).toBe(false);
  });
});
