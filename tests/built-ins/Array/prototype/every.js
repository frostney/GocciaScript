/*---
description: Array.prototype.every checks if all elements in the array satisfy a condition
features: [Array.prototype.every]
---*/

test("Array.prototype.every checks if all elements in the array satisfy a condition", () => {
  const arr = [1, 2, 3];
  const every = arr.every((x) => x > 0);
  expect(every).toBe(true);
});

test("Array.prototype.every with empty array", () => {
  const arr = [];
  const every = arr.every((x) => x > 0);
  expect(every).toBe(true);
});
