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
});
