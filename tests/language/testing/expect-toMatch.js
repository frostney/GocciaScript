/*---
description: expect(...).toMatch supports strings and regular expressions
features: [Test Assertions, RegExp]
---*/

test("toMatch accepts string patterns and RegExp objects", () => {
  expect("hello world").toMatch("world");
  expect("hello world").toMatch(/wo.ld/);
  expect("hello world").not.toMatch(/z+/);
});

test("toMatch ignores RegExp lastIndex state", () => {
  const regex = /el/g;
  regex.lastIndex = 2;

  expect("hello").toMatch(regex);
  expect(regex.lastIndex).toBe(2);
});
