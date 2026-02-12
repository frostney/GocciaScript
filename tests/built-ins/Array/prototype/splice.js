/*---
description: Array.prototype.splice (mutating)
features: [Array.prototype.splice]
---*/

describe("Array.prototype.splice", () => {
  test("remove elements", () => {
    const arr = [1, 2, 3, 4, 5];
    const removed = arr.splice(1, 2);
    expect(removed).toEqual([2, 3]);
    expect(arr).toEqual([1, 4, 5]);
  });

  test("insert elements", () => {
    const arr = [1, 4, 5];
    const removed = arr.splice(1, 0, 2, 3);
    expect(removed).toEqual([]);
    expect(arr).toEqual([1, 2, 3, 4, 5]);
  });

  test("replace elements", () => {
    const arr = [1, 2, 3, 4, 5];
    const removed = arr.splice(1, 2, "a", "b", "c");
    expect(removed).toEqual([2, 3]);
    expect(arr).toEqual([1, "a", "b", "c", 4, 5]);
  });

  test("negative start index", () => {
    const arr = [1, 2, 3, 4, 5];
    const removed = arr.splice(-2, 1);
    expect(removed).toEqual([4]);
    expect(arr).toEqual([1, 2, 3, 5]);
  });

  test("delete to end when no deleteCount", () => {
    const arr = [1, 2, 3, 4, 5];
    const removed = arr.splice(2);
    expect(removed).toEqual([3, 4, 5]);
    expect(arr).toEqual([1, 2]);
  });

  test("empty splice returns empty array", () => {
    const arr = [1, 2, 3];
    const removed = arr.splice(1, 0);
    expect(removed).toEqual([]);
    expect(arr).toEqual([1, 2, 3]);
  });
});
