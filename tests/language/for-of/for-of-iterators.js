/*---
description: for...of with custom iterators and built-in collections
features: [for-of]
---*/

describe("for...of with iterators", () => {
  test("iterates over Set", () => {
    const s = new Set([10, 20, 30]);
    const result = [];
    for (const item of s) {
      result.push(item);
    }
    expect(result).toEqual([10, 20, 30]);
  });

  test("iterates over Map", () => {
    const m = new Map([["a", 1], ["b", 2]]);
    const keys = [];
    const values = [];
    for (const [key, value] of m) {
      keys.push(key);
      values.push(value);
    }
    expect(keys).toEqual(["a", "b"]);
    expect(values).toEqual([1, 2]);
  });

  test("iterates over custom iterable", () => {
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
    const result = [];
    for (const item of iterable) {
      result.push(item);
    }
    expect(result).toEqual([10, 20, 30]);
  });

  test("non-iterable throws TypeError", () => {
    expect(() => {
      for (const item of 42) {
        // should not reach
      }
    }).toThrow(TypeError);
  });
});
