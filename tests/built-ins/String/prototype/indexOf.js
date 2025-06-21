/*---
description: String.prototype.indexOf works correctly
features: [String.prototype.indexOf]
---*/

test("String.prototype.indexOf finds strings", () => {
  const str = "hello world";
  expect(str.indexOf("hello")).toBe(0);
  expect(str.indexOf("world")).toBe(6);
  expect(str.indexOf("xyz")).toBe(-1);
  expect(str.indexOf("hello", 1)).toBe(-1);
  expect(str.indexOf("hello", 12)).toBe(-1);
  expect(str.indexOf("hello", 13)).toBe(-1);
  expect(str.indexOf("hello", 14)).toBe(-1);
  expect(str.indexOf("hello", 15)).toBe(-1);
  expect(str.indexOf("hello", 16)).toBe(-1);
});
