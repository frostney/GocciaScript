/*---
description: Object.keys
features: [Object.keys]
---*/

test("Object.keys", () => {
  const obj = { a: 1, b: 2, c: 3 };
  expect(Object.keys(obj)).toEqual(["a", "b", "c"]);
});
