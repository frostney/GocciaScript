/*---
description: Object.values
features: [Object.values]
---*/

test("Object.values", () => {
  const obj = { a: 1, b: 2, c: 3 };
  expect(Object.values(obj)).toEqual([1, 2, 3]);
});
