/*---
description: Array.prototype.shift and Array.prototype.unshift
features: [Array.prototype.shift, Array.prototype.unshift]
---*/

describe("Array.prototype.shift", () => {
  test("removes and returns the first element", () => {
    const arr = [1, 2, 3];
    const first = arr.shift();
    expect(first).toBe(1);
    expect(arr).toEqual([2, 3]);
  });

  test("returns undefined for empty array", () => {
    const arr = [];
    expect(arr.shift()).toBe(undefined);
  });

  test("works with single element", () => {
    const arr = ["only"];
    expect(arr.shift()).toBe("only");
    expect(arr.length).toBe(0);
  });
});

describe("Array.prototype.unshift", () => {
  test("adds elements to the beginning", () => {
    const arr = [3, 4, 5];
    const newLen = arr.unshift(1, 2);
    expect(newLen).toBe(5);
    expect(arr).toEqual([1, 2, 3, 4, 5]);
  });

  test("adds single element", () => {
    const arr = [2, 3];
    arr.unshift(1);
    expect(arr).toEqual([1, 2, 3]);
  });

  test("works on empty array", () => {
    const arr = [];
    arr.unshift("a", "b");
    expect(arr).toEqual(["a", "b"]);
  });

  test("unshift returns new length", () => {
    const arr = [3];
    const len = arr.unshift(1, 2);
    expect(len).toBe(3);
  });

  test("unshift with no arguments returns current length", () => {
    const arr = [1, 2, 3];
    expect(arr.unshift()).toBe(3);
  });
});
