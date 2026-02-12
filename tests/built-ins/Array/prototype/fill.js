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
});
