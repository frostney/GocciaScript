/*---
description: String.prototype.lastIndexOf works correctly
features: [String.prototype.lastIndexOf]
---*/

test("String.prototype.lastIndexOf finds strings", () => {
  const str = "hello world hello";
  expect(str.lastIndexOf("hello")).toBe(12);
  expect(str.lastIndexOf("world")).toBe(6);
  expect(str.lastIndexOf("xyz")).toBe(-1);
  expect(str.lastIndexOf("hello", 1)).toBe(0);
  expect(str.lastIndexOf("hello", 12)).toBe(12);
  expect(str.lastIndexOf("hello", 13)).toBe(12);
  expect(str.lastIndexOf("hello", 14)).toBe(12);
});
