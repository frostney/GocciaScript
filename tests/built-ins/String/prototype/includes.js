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
  expect(str.includes("world", 6)).toBe(true);
  expect(str.includes("hello", 12)).toBe(false); // Position beyond string length
  expect(str.includes("", 12)).toBe(true); // Empty string can be found at any position
  expect(str.includes("", 100)).toBe(true); // Empty string can be found at any position
  expect(str.includes("o", 4)).toBe(true); // 'o' at position 4
});
