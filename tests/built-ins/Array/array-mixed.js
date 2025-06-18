/*---
description: Array with mixed types
features: [Array]
---*/

test("array with mixed types", () => {
  const arr = [1, "two", true, null, undefined, { a: 1 }];
  expect(arr.length).toBe(6);
  expect(arr[0]).toBe(1);
  expect(arr[1]).toBe("two");
  expect(arr[2]).toBe(true);
  expect(arr[3]).toBeNull();
  expect(arr[4]).toBeUndefined();
  expect(arr[5].a).toBe(1);
});
