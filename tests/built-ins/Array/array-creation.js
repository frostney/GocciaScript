/*---
description: Basic array creation and element access works correctly
features: [Array]
---*/

test("basic array creation and access", () => {
  const arr = [1, 2, 3, 4, 5];
  expect(arr.length).toBe(5);
  expect(arr[0]).toBe(1);
  expect(arr[4]).toBe(5);
  expect(arr[10]).toBeUndefined();
});

test("array with object elements", () => {
  const arr = [{ a: 1 }, { b: 2 }, { c: 3 }];
  expect(arr.length).toBe(3);
  expect(arr[0].a).toBe(1);
  expect(arr[1].b).toBe(2);
  expect(arr[2].c).toBe(3);
  expect(arr[3]).toBeUndefined();
});

test("empty array", () => {
  const arr = [];
  expect(arr.length).toBe(0);
});

test("array with one element", () => {
  const arr = [1];
  expect(arr.length).toBe(1);
  expect(arr[0]).toBe(1);
});
