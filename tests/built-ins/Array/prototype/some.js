/*---
description: Array.prototype.some checks if at least one element in the array satisfies a condition
features: [Array.prototype.some]
---*/

test("Array.prototype.some checks if at least one element in the array satisfies a condition", () => {
  const arr = [1, 2, 3];
  const some = arr.some((x) => x > 2);
  expect(some).toBe(true);
});

test("Array.prototype.some with empty array", () => {
  const arr = [];
  const some = arr.some((x) => x > 2);
  expect(some).toBe(false);
});
