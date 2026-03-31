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

  test("iterates over Set in insertion order", () => {
    const set = new Set();
    set.add("c");
    set.add("a");
    set.add("b");

    const result = [];
    for (const item of set) {
      result.push(item);
    }

    expect(result).toEqual(["c", "a", "b"]);
  });

  test("iterates over Map entries", () => {
    const map = new Map();
    map.set("a", 1);
    map.set("b", 2);

    const entries = [];

    for (const entry of map) {
      entries.push(entry);
    }

    expect(entries).toEqual([["a", 1], ["b", 2]]);
  });

  test("iterates over Map keys", () => {
    const map = new Map();
    map.set("a", 1);
    map.set("b", 2);

    const keys = [];

    for (const key of map.keys()) {
      keys.push(key);
    }

    expect(keys).toEqual(["a", "b"]);
  });

  test("iterates over Map values", () => {
    const map = new Map();
    map.set("a", 1);
    map.set("b", 2);

    const values = [];

    for (const value of map.values()) {
      values.push(value);
    }

    expect(values).toEqual([1, 2]);
  });

  test("custom iterables can finish immediately", () => {
    const empty = {
      [Symbol.iterator]() {
        return {
          next() {
            return { value: undefined, done: true };
          }
        };
      }
    };

    const items = [{ id: 1 }, { id: 2 }];
    const iterable = {
      [Symbol.iterator]() {
        let i = 0;
        return {
          next() {
            if (i < items.length) {
              const value = items[i];
              i = i + 1;
              return { value, done: false };
            }
            return { value: undefined, done: true };
          }
        };
      }
    };

    const emptyResult = [];

    for (const item of empty) {
      emptyResult.push(item);
    }

    expect(emptyResult).toEqual([]);
  });

  test("custom iterables can yield objects", () => {
    const items = [{ id: 1 }, { id: 2 }];
    const iterable = {
      [Symbol.iterator]() {
        let i = 0;
        return {
          next() {
            if (i < items.length) {
              const value = items[i];
              i = i + 1;
              return { value, done: false };
            }
            return { value: undefined, done: true };
          }
        };
      }
    };

    const ids = [];

    for (const item of iterable) {
      ids.push(item.id);
    }

    expect(ids).toEqual([1, 2]);
  });

  test("null is not iterable", () => {
    expect(() => {
      for (const item of null) {
      }
    }).toThrow(TypeError);
  });

  test("undefined is not iterable", () => {
    expect(() => {
      for (const item of undefined) {
      }
    }).toThrow(TypeError);
  });

  test("numbers are not iterable", () => {
    expect(() => {
      for (const item of 42) {
      }
    }).toThrow(TypeError);
  });

  test("booleans are not iterable", () => {
    expect(() => {
      for (const item of true) {
      }
    }).toThrow(TypeError);
  });

  test("plain objects without Symbol.iterator are not iterable", () => {
    expect(() => {
      for (const item of { a: 1 }) {
      }
    }).toThrow(TypeError);
  });
});
