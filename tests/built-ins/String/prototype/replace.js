/*---
description: String.prototype.replace works correctly
features: [String.prototype.replace]
---*/

test("String.prototype.replace replaces strings", () => {
  const str = "hello world";
  expect(str.replace("hello", "hi")).toBe("hi world");
  expect("".replace("hello", "hi")).toBe("");
  expect("hello".replace("hello", "hi")).toBe("hi");
});
