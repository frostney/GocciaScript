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

runTests();
