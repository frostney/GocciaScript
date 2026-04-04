/*---
description: test.each expands parameterized test rows
features: [Test Assertions]
---*/

test.each([
  [1, 1, 2],
  [1, 2, 3],
  [2, 3, 5],
])("adds %i + %i = %i", (a, b, expected) => {
  expect(a + b).toBe(expected);
});

test.each([
  ["alpha"],
  ["beta"],
])("passes single row values to %s", (value) => {
  expect(typeof value).toBe("string");
});
