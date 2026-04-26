/*---
description: Array.prototype.unshift
features: [Array.prototype.unshift]
---*/

describe("Array.prototype.unshift", () => {
  test("has the correct method length", () => {
    expect(Array.prototype.unshift.length).toBe(1);
  });

  test("adds elements to the beginning", () => {
    const arr = [3, 4, 5];
    const newLen = arr.unshift(1, 2);
    expect(newLen).toBe(5);
    expect(arr).toEqual([1, 2, 3, 4, 5]);
  });

  test("adds a single element", () => {
    const arr = [2, 3];
    arr.unshift(1);
    expect(arr).toEqual([1, 2, 3]);
  });

  test("works on empty arrays", () => {
    const arr = [];
    arr.unshift("a", "b");
    expect(arr).toEqual(["a", "b"]);
  });

  test("returns the new length", () => {
    const arr = [3];
    const len = arr.unshift(1, 2);
    expect(len).toBe(3);
  });

  test("with no arguments returns the current length", () => {
    const arr = [1, 2, 3];
    expect(arr.unshift()).toBe(3);
    expect(arr).toEqual([1, 2, 3]);
  });

  test("generic receiver inserts at front of array-like", () => {
    const obj = { 0: 'b', 1: 'c', length: 2 };
    const newLen = Array.prototype.unshift.call(obj, 'a');
    expect(newLen).toBe(3);
    expect(obj[0]).toBe('a');
    expect(obj[1]).toBe('b');
    expect(obj[2]).toBe('c');
    expect(obj.length).toBe(3);
  });

  test("throws TypeError when new length would exceed Number.MAX_SAFE_INTEGER", () => {
    const obj = { length: Number.MAX_SAFE_INTEGER };
    expect(() => Array.prototype.unshift.call(obj, 1)).toThrow(TypeError);
  });

  test("with no args returns the receiver's full length even when > 2^31", () => {
    const huge = { length: 4294967290 };
    const result = Array.prototype.unshift.call(huge);
    expect(result).toBe(4294967290);
    expect(huge.length).toBe(4294967290);
  });
});
