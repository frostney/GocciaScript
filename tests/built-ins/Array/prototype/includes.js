/*---
description: Array.prototype.includes checks if an array contains a value
features: [Array.prototype.includes]
---*/

test("Array.prototype.includes checks if an array contains a number value", () => {
  const arr = [1, 2, 3];
  const includes = arr.includes(2);
  const includesFour = arr.includes(4);
  expect(includes).toBe(true);
  expect(includesFour).toBe(false);
});

test("Array.prototype.includes checks if an array contains a string value", () => {
  const pets = ["cat", "dog", "bat"];
  const includes = pets.includes("cat");
  const includesAt = pets.includes("at");
  expect(includes).toBe(true);
  expect(includesAt).toBe(false);
});

test("Array.prototype.includes with fromValue", () => {
  const arr = ["cat", "dog", "bat"];
  const includes = arr.includes("cat", 1);
  const includesBat = arr.includes("bat", 1);
  const includesAt = arr.includes("at", 1);
  expect(includes).toBe(false);
  expect(includesBat).toBe(true);
  expect(includesAt).toBe(false);
});

test("Array.protoype.includes with fromIndex is greater than or equal to the array length", () => {
  const arr = ["a", "b", "c"];

  expect(arr.includes("c", 3)).toBe(false);
  expect(arr.includes("c", 100)).toBe(false);
});

test("Array.protoype.includes with fromIndex is negative", () => {
  const arr = ["a", "b", "c"];

  expect(arr.includes("a", -100)).toBe(true);
  expect(arr.includes("b", -100)).toBe(true);
  expect(arr.includes("c", -100)).toBe(true);
  expect(arr.includes("a", -2)).toBe(false);
});

test("Array.prototype.includes with empty array", () => {
  const arr = [];
  const includes = arr.includes(2);
  expect(includes).toBe(false);
});
