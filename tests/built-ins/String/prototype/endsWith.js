/*---
description: String.prototype.endsWith works correctly
features: [String.prototype.endsWith]
---*/

test("String.prototype.endsWith checks if a string ends with a substring", () => {
  const str = "hello world";
  expect(str.endsWith("world")).toBe(true);
  expect(str.endsWith("hello")).toBe(false);
  expect("".endsWith("")).toBe(true);
  expect("".endsWith("a")).toBe(false);
  expect("a".endsWith("")).toBe(true);
  expect("a".endsWith("a")).toBe(true);
  expect("a".endsWith("b")).toBe(false);
  expect("a".endsWith("ab")).toBe(false);
  expect("ab".endsWith("a")).toBe(false);
  expect("ab".endsWith("b")).toBe(true);
});
