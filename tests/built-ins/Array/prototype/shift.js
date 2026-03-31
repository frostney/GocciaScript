/*---
description: Array.prototype.shift
features: [Array.prototype.shift]
---*/

describe("Array.prototype.shift", () => {
  test("removes and returns the first element", () => {
    const arr = [1, 2, 3];
    const first = arr.shift();
    expect(first).toBe(1);
    expect(arr).toEqual([2, 3]);
  });

  test("returns undefined for empty arrays", () => {
    const arr = [];
    expect(arr.shift()).toBe(undefined);
  });

  test("works with single-element arrays", () => {
    const arr = ["only"];
    expect(arr.shift()).toBe("only");
    expect(arr.length).toBe(0);
  });
});
