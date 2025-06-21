/*---
description: String.prototype.includes works correctly
features: [String.prototype.includes]
---*/

test("String.prototype.includes finds strings", () => {
  const str = "hello world";
  expect(str.includes("hello")).toBe(true);
  expect(str.includes("world")).toBe(true);
  expect(str.includes("xyz")).toBe(false);
  expect(str.includes("hello", 1)).toBe(false);
  expect(str.includes("hello", 12)).toBe(true);
  expect(str.includes("hello", 13)).toBe(true);
  expect(str.includes("hello", 14)).toBe(true);
  expect(str.includes("hello", 15)).toBe(true);
  expect(str.includes("hello", 16)).toBe(true);
});
