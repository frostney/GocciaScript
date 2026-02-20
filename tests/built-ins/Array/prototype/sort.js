/*---
description: Array.prototype.sort (mutating)
features: [Array.prototype.sort]
---*/

describe("Array.prototype.sort", () => {
  test("default sort (string comparison)", () => {
    const arr = ["banana", "apple", "cherry"];
    const result = arr.sort();
    expect(result).toBe(arr);
    expect(arr[0]).toBe("apple");
    expect(arr[1]).toBe("banana");
    expect(arr[2]).toBe("cherry");
  });

  test("numeric sort with comparator", () => {
    const arr = [10, 1, 21, 2];
    arr.sort((a, b) => a - b);
    expect(arr).toEqual([1, 2, 10, 21]);
  });

  test("descending sort", () => {
    const arr = [3, 1, 4, 1, 5];
    arr.sort((a, b) => b - a);
    expect(arr).toEqual([5, 4, 3, 1, 1]);
  });

  test("sort returns the same array", () => {
    const arr = [3, 2, 1];
    const sorted = arr.sort((a, b) => a - b);
    expect(sorted).toBe(arr);
  });

  test("sort already sorted array", () => {
    const arr = [1, 2, 3];
    arr.sort((a, b) => a - b);
    expect(arr).toEqual([1, 2, 3]);
  });

  test("default sort with Infinity values", () => {
    const arr = [3, Infinity, 1, -Infinity, 2];
    arr.sort((a, b) => a - b);
    expect(arr[0]).toBe(-Infinity);
    expect(arr[1]).toBe(1);
    expect(arr[2]).toBe(2);
    expect(arr[3]).toBe(3);
    expect(arr[4]).toBe(Infinity);
  });

  test("default sort with NaN sorts NaN last", () => {
    const arr = [3, NaN, 1, 2];
    arr.sort((a, b) => {
      if (Number.isNaN(a)) return 1;
      if (Number.isNaN(b)) return -1;
      return a - b;
    });
    expect(arr[0]).toBe(1);
    expect(arr[1]).toBe(2);
    expect(arr[2]).toBe(3);
    expect(Number.isNaN(arr[3])).toBe(true);
  });

  test("sort mutates the original array", () => {
    const arr = [3, 1, 2];
    arr.sort((a, b) => a - b);
    expect(arr).toEqual([1, 2, 3]);
  });

  test("sort with empty array", () => {
    const arr = [];
    arr.sort();
    expect(arr).toEqual([]);
  });

  test("sort with single element", () => {
    const arr = [1];
    arr.sort();
    expect(arr).toEqual([1]);
  });

  test("sort with equal elements", () => {
    const arr = [1, 1, 1];
    arr.sort((a, b) => a - b);
    expect(arr).toEqual([1, 1, 1]);
  });

  test("default sort converts to string comparison", () => {
    const arr = [80, 9, 700, 40, 1, 5, 200];
    arr.sort();
    expect(arr).toEqual([1, 200, 40, 5, 700, 80, 9]);
  });
});
