/*---
description: String.prototype.toLowerCase converts to lowercase
features: [String.prototype.toLowerCase]
---*/

test("String.prototype.toLowerCase converts to lowercase", () => {
  expect("HELLO".toLowerCase()).toBe("hello");
  expect("WoRLd".toLowerCase()).toBe("world");
  expect("".toLowerCase()).toBe("");
});
