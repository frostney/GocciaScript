/*---
description: Array.prototype.pop removes the last element from an array
features: [Array.prototype.pop]
---*/

test("Array.prototype.pop removes the last element from an array", () => {
  const arr = [1, 2, 3];

  expect(arr).toEqual([1, 2, 3]);
  expect(arr.length).toBe(3);

  const last = arr.pop();
  expect(last).toBe(3);

  expect(arr).toEqual([1, 2]);
  expect(arr.length).toEqual(2);
});
