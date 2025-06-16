/*---
description: Array.prototype.push adds elements to end of array
features: [Array.prototype.push]
---*/

test("Array.prototype.push adds element", () => {
  const arr = [1, 2];
  const newLength = arr.push(3);
  expect(newLength).toBe(3);
  expect(arr.length).toBe(3);
  expect(arr[2]).toBe(3);
});
