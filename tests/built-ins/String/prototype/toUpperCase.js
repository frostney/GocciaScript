/*---
description: String.prototype.toUpperCase basic functionality
features: [String.prototype.toUpperCase]
---*/

test("String.prototype.toUpperCase converts to uppercase", () => {
  expect("hello".toUpperCase()).toBe("HELLO");
  expect("World".toUpperCase()).toBe("WORLD");
  expect("".toUpperCase()).toBe("");
});

runTests();
