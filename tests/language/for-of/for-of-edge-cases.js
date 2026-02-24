/*---
description: >
  Edge cases for for-of loops per ES2026 §14.7.5.6 and test262
  test/language/statements/for-of/
features: [for-of]
---*/

describe("for-of with non-iterable values", () => {
  test("null throws TypeError", () => {
    expect(() => {
      for (const x of null) {}
    }).toThrow(TypeError);
  });

  test("undefined throws TypeError", () => {
    expect(() => {
      for (const x of undefined) {}
    }).toThrow(TypeError);
  });

  test("number throws TypeError", () => {
    expect(() => {
      for (const x of 42) {}
    }).toThrow(TypeError);
  });

  test("boolean throws TypeError", () => {
    expect(() => {
      for (const x of true) {}
    }).toThrow(TypeError);
  });

  test("plain object without Symbol.iterator throws TypeError", () => {
    expect(() => {
      for (const x of { a: 1 }) {}
    }).toThrow(TypeError);
  });
});

describe("for-of with empty collections", () => {
  test("empty array produces no iterations", () => {
    const results = [];
    for (const x of []) {
      results.push(x);
    }
    expect(results).toEqual([]);
  });

  test("empty string produces no iterations", () => {
    const results = [];
    for (const x of "") {
      results.push(x);
    }
    expect(results).toEqual([]);
  });

  test("empty Set produces no iterations", () => {
    const results = [];
    for (const x of new Set()) {
      results.push(x);
    }
    expect(results).toEqual([]);
  });

  test("empty Map produces no iterations", () => {
    const results = [];
    for (const x of new Map()) {
      results.push(x);
    }
    expect(results).toEqual([]);
  });
});

describe("for-of with Map methods", () => {
  test("Map iteration yields [key, value] entries", () => {
    const map = new Map();
    map.set("a", 1);
    map.set("b", 2);
    const entries = [];
    for (const entry of map) {
      entries.push(entry);
    }
    expect(entries).toEqual([["a", 1], ["b", 2]]);
  });

  test("Map with destructuring [key, value]", () => {
    const map = new Map();
    map.set("x", 10);
    map.set("y", 20);
    const keys = [];
    const values = [];
    for (const [k, v] of map) {
      keys.push(k);
      values.push(v);
    }
    expect(keys).toEqual(["x", "y"]);
    expect(values).toEqual([10, 20]);
  });

  test("Map.keys() iteration", () => {
    const map = new Map();
    map.set("a", 1);
    map.set("b", 2);
    const keys = [];
    for (const k of map.keys()) {
      keys.push(k);
    }
    expect(keys).toEqual(["a", "b"]);
  });

  test("Map.values() iteration", () => {
    const map = new Map();
    map.set("a", 1);
    map.set("b", 2);
    const values = [];
    for (const v of map.values()) {
      values.push(v);
    }
    expect(values).toEqual([1, 2]);
  });

  test("Map.entries() iteration", () => {
    const map = new Map();
    map.set("a", 1);
    map.set("b", 2);
    const entries = [];
    for (const [k, v] of map.entries()) {
      entries.push([k, v]);
    }
    expect(entries).toEqual([["a", 1], ["a", 2], ["b", 1], ["b", 2]].filter(([k, v]) => (k === "a" && v === 1) || (k === "b" && v === 2)));
  });
});

describe("for-of with Set", () => {
  test("Set iteration yields values", () => {
    const set = new Set([10, 20, 30]);
    const results = [];
    for (const x of set) {
      results.push(x);
    }
    expect(results).toEqual([10, 20, 30]);
  });

  test("Set preserves insertion order", () => {
    const set = new Set();
    set.add("c");
    set.add("a");
    set.add("b");
    const results = [];
    for (const x of set) {
      results.push(x);
    }
    expect(results).toEqual(["c", "a", "b"]);
  });

  test("Set.values() iteration", () => {
    const set = new Set([1, 2, 3]);
    const results = [];
    for (const v of set.values()) {
      results.push(v);
    }
    expect(results).toEqual([1, 2, 3]);
  });

  test("Set with mixed types", () => {
    const set = new Set([1, "two", true, null]);
    const results = [];
    for (const x of set) {
      results.push(x);
    }
    expect(results).toHaveLength(4);
    expect(results[0]).toBe(1);
    expect(results[1]).toBe("two");
    expect(results[2]).toBe(true);
    expect(results[3]).toBeNull();
  });
});

describe("for-of with strings", () => {
  test("iterates over characters", () => {
    const chars = [];
    for (const c of "abc") {
      chars.push(c);
    }
    expect(chars).toEqual(["a", "b", "c"]);
  });

  test("single character string", () => {
    const chars = [];
    for (const c of "x") {
      chars.push(c);
    }
    expect(chars).toEqual(["x"]);
  });

  test("string with spaces", () => {
    const chars = [];
    for (const c of "a b") {
      chars.push(c);
    }
    expect(chars).toEqual(["a", " ", "b"]);
  });

  test("string with special characters", () => {
    const chars = [];
    for (const c of "a\nb") {
      chars.push(c);
    }
    expect(chars).toHaveLength(3);
  });
});

describe("for-of with arrays containing holes and special values", () => {
  test("array with undefined elements", () => {
    const results = [];
    for (const x of [undefined, undefined]) {
      results.push(x);
    }
    expect(results).toHaveLength(2);
    expect(results[0]).toBeUndefined();
    expect(results[1]).toBeUndefined();
  });

  test("array with null elements", () => {
    const results = [];
    for (const x of [null, null]) {
      results.push(x);
    }
    expect(results).toHaveLength(2);
    expect(results[0]).toBeNull();
    expect(results[1]).toBeNull();
  });

  test("array with NaN", () => {
    const results = [];
    for (const x of [NaN, 1, NaN]) {
      results.push(x);
    }
    expect(results).toHaveLength(3);
    expect(results[0]).toBeNaN();
    expect(results[1]).toBe(1);
    expect(results[2]).toBeNaN();
  });

  test("array with mixed types", () => {
    const results = [];
    for (const x of [1, "two", true, null, undefined]) {
      results.push(x);
    }
    expect(results).toHaveLength(5);
    expect(results[0]).toBe(1);
    expect(results[1]).toBe("two");
    expect(results[2]).toBe(true);
    expect(results[3]).toBeNull();
    expect(results[4]).toBeUndefined();
  });

  test("single element array", () => {
    const results = [];
    for (const x of [42]) {
      results.push(x);
    }
    expect(results).toEqual([42]);
  });
});

describe("for-of with custom iterators", () => {
  test("custom iterable with finite sequence", () => {
    const range = {
      [Symbol.iterator]() {
        let current = 1;
        return {
          next() {
            if (current <= 3) {
              const value = current;
              current = current + 1;
              return { value, done: false };
            }
            return { value: undefined, done: true };
          }
        };
      }
    };
    const results = [];
    for (const x of range) {
      results.push(x);
    }
    expect(results).toEqual([1, 2, 3]);
  });

  test("custom iterable with immediate done", () => {
    const empty = {
      [Symbol.iterator]() {
        return {
          next() {
            return { value: undefined, done: true };
          }
        };
      }
    };
    const results = [];
    for (const x of empty) {
      results.push(x);
    }
    expect(results).toEqual([]);
  });

  test("custom iterable yielding objects", () => {
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
});

describe("for-of scoping", () => {
  test("const binding creates fresh binding per iteration", () => {
    const funcs = [];
    for (const x of [10, 20, 30]) {
      funcs.push(() => x);
    }
    expect(funcs[0]()).toBe(10);
    expect(funcs[1]()).toBe(20);
    expect(funcs[2]()).toBe(30);
  });

  test("let binding creates fresh binding per iteration", () => {
    const funcs = [];
    for (let x of [10, 20, 30]) {
      funcs.push(() => x);
    }
    expect(funcs[0]()).toBe(10);
    expect(funcs[1]()).toBe(20);
    expect(funcs[2]()).toBe(30);
  });
});
