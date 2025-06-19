/*---
description: Array.prototype.toSpliced
features: [Array.prototype.toSpliced]
---*/

test("Array.prototype.toSpliced with numbers", () => {
  const arr = [1, 2, 3, 4, 5];
  expect(arr.toSpliced(2, 2, 6, 7)).toEqual([1, 2, 6, 7, 5]);
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
