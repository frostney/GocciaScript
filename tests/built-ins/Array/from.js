/*---
description: Array.from
features: [Array.from]
---*/

describe("Array.from", () => {
  test("creates array from another array", () => {
    const arr = Array.from([1, 2, 3]);
    expect(arr).toEqual([1, 2, 3]);
  });

  test("creates array from string", () => {
    const arr = Array.from("hello");
    expect(arr).toEqual(["h", "e", "l", "l", "o"]);
  });

  test("empty input returns empty array", () => {
    const arr = Array.from([]);
    expect(arr).toEqual([]);
  });

  test("with map function", () => {
    const arr = Array.from([1, 2, 3], (x) => x * 2);
    expect(arr).toEqual([2, 4, 6]);
  });

  test("creates a new array (not the same reference)", () => {
    const original = [1, 2, 3];
    const copy = Array.from(original);
    expect(copy).toEqual(original);
    expect(copy === original).toBe(false);
  });

  test("Array.from with Set", () => {
    const set = new Set([1, 2, 3]);
    expect(Array.from(set)).toEqual([1, 2, 3]);
  });

  test("Array.from with Map", () => {
    const map = new Map([["a", 1], ["b", 2]]);
    const result = Array.from(map);
    expect(result).toEqual([["a", 1], ["b", 2]]);
  });

  test("Array.from with map function on Set", () => {
    const set = new Set([1, 2, 3]);
    expect(Array.from(set, (x) => x * 2)).toEqual([2, 4, 6]);
  });

  test("Array.from with array-like object", () => {
    const result = Array.from({ length: 3, 0: "a", 1: "b", 2: "c" });
    expect(result).toEqual(["a", "b", "c"]);
  });

  test("Array.from with array-like object with gaps", () => {
    const result = Array.from({ length: 3, 0: "a", 2: "c" });
    expect(result.length).toBe(3);
    expect(result[0]).toBe("a");
    expect(result[1]).toBe(undefined);
    expect(result[2]).toBe("c");
  });

  test("Array.from map function receives index as second argument", () => {
    const indices = [];
    Array.from([10, 20, 30], (val, idx) => {
      indices.push(idx);
      return val;
    });
    expect(indices).toEqual([0, 1, 2]);
  });

  test("Array.from with length 0 array-like", () => {
    expect(Array.from({ length: 0 })).toEqual([]);
  });

  test("Array.from does not call map function for empty source", () => {
    let called = false;
    Array.from([], () => {
      called = true;
    });
    expect(called).toBe(false);
  });
});
