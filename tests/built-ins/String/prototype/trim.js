/*---
description: String.prototype.trim works correctly
features: [String.prototype.trim]
---*/

test("String.prototype.trim removes whitespace", () => {
  const str = "  hello world  ";
  expect(str.trim()).toBe("hello world");
  expect("".trim()).toBe("");
  expect(" ".trim()).toBe("");
  expect("  ".trim()).toBe("");
  expect("  hello world".trim()).toBe("hello world");
  expect("hello world  ".trim()).toBe("hello world");
});
