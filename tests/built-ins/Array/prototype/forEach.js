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

test("forEach returns undefined always", () => {
  const result = [1, 2, 3].forEach((x) => x * 2);
  expect(result).toBe(undefined);
});

test("forEach calls callback for each element in order", () => {
  const collected = [];
  [10, 20, 30].forEach((val) => {
    collected.push(val);
  });
  expect(collected).toEqual([10, 20, 30]);
});

test("forEach callback count matches array length", () => {
  let count = 0;
  [1, 2, 3, 4, 5].forEach(() => {
    count = count + 1;
  });
  expect(count).toBe(5);
});
