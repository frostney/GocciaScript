/*---
description: Array.prototype.slice
features: [Array.prototype.slice]
---*/

test("Array.protoype.slice copies the array from the start index", () => {
  const arr = [1, 2, 3, 4, 5];
  const sliced = arr.slice(2);
  expect(sliced).toEqual([3, 4, 5]);
  expect(sliced).not.toBe(arr);
});

test("Array.prototype.slice copies the array with negative start index", () => {
  const fruits = ["Apple", "Banana", "Orange", "Mango", "Pineapple"];

  const lastTwo = fruits.slice(-2);
  expect(lastTwo).toEqual(["Mango", "Pineapple"]);
});

test("Array.protoype.slice copies the array from the start index to the end index", () => {
  const arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
  const sliced = arr.slice(2, 8);
  expect(sliced).toEqual([3, 4, 5, 6, 7, 8]);
  expect(sliced).not.toBe(arr);
});

test("Array.prototype.slice copies the array with positive start index and negative end index", () => {
  const fruits = ["Apple", "Banana", "Orange", "Mango", "Pineapple"];

  const sliceExample = fruits.slice(1, -1);
  expect(sliceExample).toEqual(["Banana", "Orange", "Mango"]);
});

test("Array.prototype.slice shallowly copies the array", () => {
  const arr = [1, 2, 3, 4, 5];
  const sliced = arr.slice();
  expect(sliced).toEqual([1, 2, 3, 4, 5]);
  expect(sliced).not.toBe(arr);
});
