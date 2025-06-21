/*---
description: String.prototype.startsWith works correctly
features: [String.prototype.startsWith]
---*/

test("String.prototype.startsWith checks if a string starts with a substring", () => {
  const str = "hello world";
  expect(str.startsWith("hello")).toBe(true);
  expect(str.startsWith("world")).toBe(false);
  expect("".startsWith("")).toBe(true);
  expect("".startsWith("a")).toBe(false);
  expect("a".startsWith("")).toBe(true);
  expect("a".startsWith("a")).toBe(true);
  expect("a".startsWith("b")).toBe(false);
  expect("a".startsWith("ab")).toBe(false);
  expect("ab".startsWith("a")).toBe(true);
  expect("ab".startsWith("b")).toBe(false);
});
