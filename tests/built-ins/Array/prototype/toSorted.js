/*---
description: Array.prototype.toSorted returns a new array with the elements sorted
features: [Array.prototype.toSorted]
---*/

test("Array.prototype.toSorted returns a new array with the elements sorted", () => {
  const arr = [3, 1, 2];
  const sorted = arr.toSorted();
  expect(arr).toEqual([3, 1, 2]);
  expect(sorted).toEqual([1, 2, 3]);
});

test("Array.prototype.toSorted with empty array", () => {
  const arr = [];
  const sorted = arr.toSorted();
  expect(sorted).toEqual([]);
});

test("Array.prototype.toSorted with custom sort function", () => {
  const arr = [3, 1, 2, 4, 5];
  const sorted = arr.toSorted((a, b) => b - a);
  expect(arr).toEqual([3, 1, 2, 4, 5]);
  expect(sorted).toEqual([5, 4, 3, 2, 1]);

  const arr2 = [3, 1, 2, 4, 5];
  const sorted2 = arr2.toSorted((a, b) => a - b);
  expect(arr2).toEqual([3, 1, 2, 4, 5]);
  expect(sorted2).toEqual([1, 2, 3, 4, 5]);
});
