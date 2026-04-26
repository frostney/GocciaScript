/*---
description: Array.prototype.toSpliced
features: [Array.prototype.toSpliced]
---*/

test("Array.prototype.toSpliced with numbers", () => {
  const arr = [1, 2, 3, 4, 5];
  expect(arr.toSpliced(2, 2, 6, 7)).toEqual([1, 2, 6, 7, 5]);
});

test("Array.prototype.toSpliced has the correct method length", () => {
  expect(Array.prototype.toSpliced.length).toBe(2);
});

test("Array.prototype.toSpliced with strings", () => {
  const months = ["Jan", "Mar", "Apr", "May"];

  // Inserting an element at index 1
  const months2 = months.toSpliced(1, 0, "Feb");
  expect(months2).toEqual(["Jan", "Feb", "Mar", "Apr", "May"]);

  // Deleting two elements starting from index 2
  const months3 = months2.toSpliced(2, 2);
  expect(months3).toEqual(["Jan", "Feb", "May"]);

  // Replacing one element at index 1 with two new elements
  const months4 = months3.toSpliced(1, 1, "Feb", "Mar");
  expect(months4).toEqual(["Jan", "Feb", "Mar", "May"]);

  // Original array is not modified
  expect(months).toEqual(["Jan", "Mar", "Apr", "May"]);
});

test("Array.prototype.toSpliced with sparse array", () => {
  const arr = [1, , 3, 4, , 6];
  expect(arr.toSpliced(1, 2)).toEqual([1, 4, undefined, 6]);
});

test("Array.prototype.toSpliced with empty array", () => {
  const arr = [];
  expect(arr.toSpliced(0, 0, 1)).toEqual([1]);
});

test("generic receiver creates spliced copy from array-like", () => {
  const arrayLike = { 0: 'a', 1: 'b', 2: 'c', length: 3 };
  const result = Array.prototype.toSpliced.call(arrayLike, 1, 1, 'x');
  expect(result).toEqual(['a', 'x', 'c']);
});

test("toSpliced throws TypeError when newLen would exceed Number.MAX_SAFE_INTEGER", () => {
  const obj = { length: Number.MAX_SAFE_INTEGER };
  expect(() => Array.prototype.toSpliced.call(obj, 0, 0, 1)).toThrow(TypeError);
});

test("toSpliced throws RangeError when newLen exceeds 2**32 - 1", () => {
  const obj = { length: 2 ** 32 };
  expect(() => Array.prototype.toSpliced.call(obj, 0, 0)).toThrow(RangeError);
});

test("toSpliced throws RangeError when skipCount exceeds engine MaxInt on huge receiver", () => {
  // toSpliced(0, 2^40 - 100) on a length-2^40 receiver: NewLen would fit
  // in MaxInt (=100), but the skip count itself overflows the read-loop
  // Integer counter.  Reject up-front rather than wrapping.
  const obj = { length: 2 ** 40 };
  expect(() => Array.prototype.toSpliced.call(obj, 0, 2 ** 40 - 100))
    .toThrow(RangeError);
});
