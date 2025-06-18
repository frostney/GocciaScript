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
