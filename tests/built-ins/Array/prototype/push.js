/*---
description: Array.prototype.push adds elements to end of array
features: [Array.prototype.push]
---*/

describe("Array.prototype.push", () => {
  test("adds element and returns new length", () => {
    const arr = [1, 2];
    const newLength = arr.push(3);
    expect(newLength).toBe(3);
    expect(arr.length).toBe(3);
    expect(arr[2]).toBe(3);
  });

  test("push multiple elements", () => {
    const arr = [1];
    const len = arr.push(2, 3, 4);
    expect(len).toBe(4);
    expect(arr).toEqual([1, 2, 3, 4]);
  });

  test("push with no arguments returns current length", () => {
    const arr = [1, 2, 3];
    expect(arr.push()).toBe(3);
    expect(arr).toEqual([1, 2, 3]);
  });

  test("push on empty array", () => {
    const arr = [];
    expect(arr.push(1)).toBe(1);
    expect(arr).toEqual([1]);
  });

  test("push null and undefined", () => {
    const arr = [];
    arr.push(null, undefined);
    expect(arr).toEqual([null, undefined]);
  });
});
