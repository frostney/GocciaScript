/*---
description: Array.prototype.fill
features: [Array.prototype.fill]
---*/

describe("Array.prototype.fill", () => {
  test("fill entire array", () => {
    const arr = [1, 2, 3, 4];
    arr.fill(0);
    expect(arr).toEqual([0, 0, 0, 0]);
  });

  test("fill with start index", () => {
    const arr = [1, 2, 3, 4];
    arr.fill(0, 2);
    expect(arr).toEqual([1, 2, 0, 0]);
  });

  test("fill with start and end index", () => {
    const arr = [1, 2, 3, 4];
    arr.fill(0, 1, 3);
    expect(arr).toEqual([1, 0, 0, 4]);
  });

  test("fill with negative indices", () => {
    const arr = [1, 2, 3, 4];
    arr.fill(0, -2);
    expect(arr).toEqual([1, 2, 0, 0]);
  });

  test("fill returns the modified array", () => {
    const arr = [1, 2, 3];
    const result = arr.fill(0);
    expect(result).toBe(arr);
  });

  test("fill with start >= end does nothing", () => {
    const arr = [1, 2, 3];
    arr.fill(0, 2, 1);
    expect(arr).toEqual([1, 2, 3]);
  });

  test("fill with start beyond length does nothing", () => {
    const arr = [1, 2, 3];
    arr.fill(0, 100);
    expect(arr).toEqual([1, 2, 3]);
  });

  test("fill with negative start and end", () => {
    const arr = [1, 2, 3, 4, 5];
    arr.fill(0, -3, -1);
    expect(arr).toEqual([1, 2, 0, 0, 5]);
  });

  test("fill on empty array", () => {
    const arr = [];
    arr.fill(0);
    expect(arr).toEqual([]);
  });

  test("fill with undefined value", () => {
    const arr = [1, 2, 3];
    arr.fill(undefined);
    expect(arr).toEqual([undefined, undefined, undefined]);
  });
});
