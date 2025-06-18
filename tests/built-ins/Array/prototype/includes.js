/*---
description: Array.prototype.includes checks if an array contains a value
features: [Array.prototype.includes]
---*/

test("Array.prototype.includes checks if an array contains a value", () => {
  const arr = [1, 2, 3];
  const includes = arr.includes(2);
  const includesFail = arr.includes(4);
  expect(includes).toBe(true);
  expect(includesFail).toBe(false);
});

test("Array.prototype.includes with empty array", () => {
  const arr = [];
  const includes = arr.includes(2);
  expect(includes).toBe(false);
});
