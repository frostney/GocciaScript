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
});
