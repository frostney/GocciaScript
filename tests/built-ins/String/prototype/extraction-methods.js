/*---
description: String extraction methods work correctly
features: [String.prototype.substring, String.prototype.slice, String.prototype.substr]
---*/

test("string extraction methods", () => {
  const str = "hello world";
  expect(str.substring(0, 5)).toBe("hello");
  expect(str.substring(6)).toBe("world");
  expect(str.slice(0, 5)).toBe("hello");
  expect(str.slice(-5)).toBe("world");
});
