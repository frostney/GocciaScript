/*---
description: Array.prototype.toReversed returns a new array with the elements in reverse order
features: [Array.prototype.toReversed]
---*/

test("Array.prototype.toReversed returns a new array with the elements in reverse order", () => {
  const arr = [1, 2, 3];
  const reversed = arr.toReversed();
  expect(reversed).toEqual([3, 2, 1]);

  const arr2 = [5, 4, 3, 2, 1];
  const reversed2 = arr2.toReversed();
  expect(reversed2).toEqual([1, 2, 3, 4, 5]);
});

test("Array.prototype.toReversed with empty array", () => {
  const arr = [];
  const reversed = arr.toReversed();
  expect(reversed).toEqual([]);
});

test("toReversed does not mutate the original array", () => {
  const arr = [1, 2, 3];
  const reversed = arr.toReversed();
  expect(arr).toEqual([1, 2, 3]);
  expect(reversed).toEqual([3, 2, 1]);
  expect(reversed).not.toBe(arr);
});

test("toReversed with single element", () => {
  expect([42].toReversed()).toEqual([42]);
});

test("generic receiver creates reversed copy from array-like", () => {
  const arrayLike = { 0: 'a', 1: 'b', 2: 'c', length: 3 };
  const result = Array.prototype.toReversed.call(arrayLike);
  expect(result).toEqual(['c', 'b', 'a']);
  expect(arrayLike[0]).toBe('a');
  expect(arrayLike[1]).toBe('b');
  expect(arrayLike[2]).toBe('c');
  expect(arrayLike.length).toBe(3);
});

test("toReversed throws RangeError when length exceeds 2**32 - 1", () => {
  const obj = { length: 2 ** 32 };
  expect(() => Array.prototype.toReversed.call(obj)).toThrow(RangeError);
});

test("toReversed throws RangeError on Number.MAX_SAFE_INTEGER length", () => {
  const obj = { length: Number.MAX_SAFE_INTEGER };
  expect(() => Array.prototype.toReversed.call(obj)).toThrow(RangeError);
});
