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

  test("generic receiver removes and inserts in array-like", () => {
    const obj = { 0: 'a', 1: 'b', 2: 'c', length: 3 };
    const removed = Array.prototype.splice.call(obj, 1, 1, 'x', 'y');
    expect(removed).toEqual(['b']);
    expect(obj[0]).toBe('a');
    expect(obj[1]).toBe('x');
    expect(obj[2]).toBe('y');
    expect(obj[3]).toBe('c');
    expect(obj.length).toBe(4);
  });

  test("splice returns sparse removed array", () => {
    const arr = [1, , 3];
    const removed = arr.splice(0, 3);
    expect(removed.length).toBe(3);
    expect(0 in removed).toBe(true);
    expect(1 in removed).toBe(false);
    expect(2 in removed).toBe(true);
  });

  test("throws TypeError when new length would exceed Number.MAX_SAFE_INTEGER", () => {
    const obj = { length: Number.MAX_SAFE_INTEGER };
    expect(() => Array.prototype.splice.call(obj, 0, 0, 1)).toThrow(TypeError);
  });
});
