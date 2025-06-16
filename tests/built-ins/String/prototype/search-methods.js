/*---
description: String search methods work correctly
features: [String.prototype.indexOf, String.prototype.lastIndexOf, String.prototype.includes]
---*/

test("string search methods", () => {
  const str = "hello world hello";
  expect(str.indexOf("hello")).toBe(0);
  expect(str.indexOf("hello", 1)).toBe(12);
  expect(str.indexOf("xyz")).toBe(-1);
  expect(str.lastIndexOf("hello")).toBe(12);
  expect(str.includes("world")).toBeTruthy();
  expect(str.includes("xyz")).toBeFalsy();
});

runTests();
