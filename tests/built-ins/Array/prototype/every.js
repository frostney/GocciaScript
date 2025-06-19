/*---
description: Array.prototype.every checks if all elements in the array satisfy a condition
features: [Array.prototype.every]
---*/

test("Array.prototype.every checks if all elements in the array satisfy a condition", () => {
  const arr = [1, 2, 3];
  const every = arr.every((x) => x > 0);
  expect(every).toBe(true);
});

it("Array.protoype.every with index and array parameters", () => {
  const arr = [1, 2, 3];
  const every = arr.every((x, index, array) => {
    expect(array).toBe(arr);

    if (index === 0) {
      expect(x).toBe(1);
      expect(index).toBe(0);
    }

    if (index === 1) {
      expect(x).toBe(2);
      expect(index).toBe(1);
    }

    if (index === 2) {
      expect(x).toBe(3);
      expect(index).toBe(2);
    }

    return x > 0;
  });
  expect(every).toBe(true);
});

test("Array.prototype.every with empty array", () => {
  const arr = [];
  const every = arr.every((x) => x > 0);
  expect(every).toBe(true);
});

test("Array.prototype.every to check array subset", () => {
  const isSubset = (array1, array2) =>
    array2.every((element) => array1.includes(element));

  expect(isSubset([1, 2, 3, 4, 5, 6, 7], [5, 7, 6])).toBe(true);
  expect(isSubset([1, 2, 3, 4, 5, 6, 7], [5, 8, 7])).toBe(false);
});

test("Array.prototype.every with sparse array", () => {
  expect([1, , 3].every((x) => x !== undefined)).toBe(true);
  expect([2, , 2].every((x) => x === 2)).toBe(true);
});
