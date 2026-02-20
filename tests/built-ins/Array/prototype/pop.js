/*---
description: Array.prototype.pop removes the last element from an array
features: [Array.prototype.pop]
---*/

describe("Array.prototype.pop", () => {
  test("removes and returns the last element", () => {
    const arr = [1, 2, 3];
    const last = arr.pop();
    expect(last).toBe(3);
    expect(arr).toEqual([1, 2]);
    expect(arr.length).toBe(2);
  });

  test("pop on empty array returns undefined", () => {
    const arr = [];
    expect(arr.pop()).toBe(undefined);
    expect(arr.length).toBe(0);
  });

  test("pop on single element array", () => {
    const arr = [42];
    expect(arr.pop()).toBe(42);
    expect(arr.length).toBe(0);
    expect(arr).toEqual([]);
  });

  test("successive pops empty the array", () => {
    const arr = [1, 2];
    arr.pop();
    arr.pop();
    expect(arr.length).toBe(0);
    expect(arr.pop()).toBe(undefined);
  });
});
