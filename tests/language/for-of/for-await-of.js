/*---
description: for-await-of with async iterables
features: [for-of, async-await]
---*/

describe("for-await-of", () => {
  test("iterates over async iterable", () => {
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

    const fn = async () => {
      const result = [];
      for await (const item of asyncIterable) {
        result.push(item);
      }
      return result;
    };

    return fn().then((result) => {
      expect(result).toEqual([1, 2, 3]);
    });
  });

  test("for-await-of falls back to sync iterable", () => {
    const fn = async () => {
      const result = [];
      for await (const item of [10, 20, 30]) {
        result.push(item);
      }
      return result;
    };

    return fn().then((result) => {
      expect(result).toEqual([10, 20, 30]);
    });
  });

  test("for-await-of awaits Promise values from sync iterator", () => {
    const fn = async () => {
      const promises = [Promise.resolve(1), Promise.resolve(2), Promise.resolve(3)];
      const result = [];
      for await (const item of promises) {
        result.push(item);
      }
      return result;
    };

    return fn().then((result) => {
      expect(result).toEqual([1, 2, 3]);
    });
  });

  test("break in for-await-of", () => {
    const asyncIterable = {
      [Symbol.asyncIterator]() {
        let i = 0;
        return {
          next() {
            i = i + 1;
            return Promise.resolve({
              value: i,
              done: i > 10
            });
          }
        };
      }
    };

    const fn = async () => {
      const result = [];
      for await (const item of asyncIterable) {
        if (item === 3) {
          break;
        }
        result.push(item);
      }
      return result;
    };

    return fn().then((result) => {
      expect(result).toEqual([1, 2]);
    });
  });

  test("non-iterable values reject with TypeError", async () => {
    await expect((async () => {
      for await (const x of null) {
      }
    })()).rejects.toThrow(TypeError);

    await expect((async () => {
      for await (const x of undefined) {
      }
    })()).rejects.toThrow(TypeError);

    await expect((async () => {
      for await (const x of 42) {
      }
    })()).rejects.toThrow(TypeError);

    await expect((async () => {
      for await (const x of true) {
      }
    })()).rejects.toThrow(TypeError);
  });

  test("rejected Promise values from sync iterables throw", async () => {
    const values = [Promise.resolve(1), Promise.reject("fail"), Promise.resolve(3)];
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

  test("async iterables can reject or finish immediately", async () => {
    const rejectingIterable = {
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

    const emptyIterable = {
      [Symbol.asyncIterator]() {
        return {
          next() {
            return Promise.resolve({ value: undefined, done: true });
          }
        };
      }
    };

    const result = [];
    try {
      for await (const value of rejectingIterable) {
        result.push(value);
      }
    } catch (e) {
      expect(e).toBe("iteration error");
      expect(result).toEqual([1]);
    }

    const emptyResult = [];
    for await (const value of emptyIterable) {
      emptyResult.push(value);
    }
    expect(emptyResult).toEqual([]);
  });

  test("for-await-of falls back to sync iterables and mixed values", async () => {
    const map = new Map();
    map.set("a", 1);
    map.set("b", 2);

    const set = new Set([10, 20, 30]);
    const mixed = [];
    const entries = [];
    const setValues = [];

    for await (const value of [Promise.resolve(1), 2, Promise.resolve(3), 4]) {
      mixed.push(value);
    }

    for await (const entry of map) {
      entries.push(entry);
    }

    for await (const value of set) {
      setValues.push(value);
    }

    expect(mixed).toEqual([1, 2, 3, 4]);
    expect(entries).toEqual([["a", 1], ["b", 2]]);
    expect(setValues).toEqual([10, 20, 30]);
  });

  test("for-await-of supports destructuring", async () => {
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
});
