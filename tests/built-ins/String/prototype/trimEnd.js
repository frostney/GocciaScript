/*---
description: String.prototype.trimEnd works correctly
features: [String.prototype.trimEnd]
---*/

test("String.prototype.trimEnd removes whitespace", () => {
  const str = "  hello world  ";
  expect(str.trimEnd()).toBe("  hello world");
  expect("".trimEnd()).toBe("");
  expect(" ".trimEnd()).toBe("");
  expect("  ".trimEnd()).toBe("");
  expect("  hello world".trimEnd()).toBe("  hello world");
  expect("hello world  ".trimEnd()).toBe("hello world");
});
