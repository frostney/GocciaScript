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
});
