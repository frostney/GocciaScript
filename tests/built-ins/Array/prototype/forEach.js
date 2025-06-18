/*---
description: Array.prototype.forEach calls a function for each element in the array
features: [Array.prototype.forEach]
---*/

test("Array.prototype.forEach calls a function for each element in the array", () => {
  const arr = [1, 2, 3];
  const mapped = arr.forEach((x) => x * 2);
  expect(mapped).toBeUndefined();
});

test("Array.prototype.forEach with arrow function block", () => {
  const arr = [1, 2, 3];
  const mapped = arr.forEach((x) => {
    return x * 2;
  });
  expect(mapped).toBeUndefined();
});

test("Array.prototype.forEach with arrow function block", () => {
  const arr = [1, 2, 3];
  const mapped = arr.forEach((value, index, array) => {
    expect(array).toBe(arr);

    if (index === 0) {
      expect(value).toBe(1);
      expect(index).toBe(0);
    }

    if (index === 1) {
      expect(value).toBe(2);
      expect(index).toBe(1);
    }

    if (index === 2) {
      expect(value).toBe(3);
      expect(index).toBe(2);
    }
  });
  expect(mapped).toBeUndefined();
});

test("Array.prototype.forEach with empty array", () => {
  const arr = [];
  const mapped = arr.forEach((x) => x * 2);
  expect(mapped).toBeUndefined();
});
