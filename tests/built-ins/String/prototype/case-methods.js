/*---
description: String case methods work correctly
features: [String.prototype.toLowerCase, String.prototype.toUpperCase]
---*/

test("string case methods", () => {
  const str = "Hello World";
  expect(str.toLowerCase()).toBe("hello world");
  expect(str.toUpperCase()).toBe("HELLO WORLD");
  expect(str).toBe("Hello World"); // Original unchanged
});
