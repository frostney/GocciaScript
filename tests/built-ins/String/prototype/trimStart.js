/*---
description: String.prototype.trimStart works correctly
features: [String.prototype.trimStart]
---*/

test("String.prototype.trimStart removes whitespace", () => {
  const str = "  hello world  ";
  expect(str.trimStart()).toBe("hello world  ");
  expect("".trimStart()).toBe("");
  expect(" ".trimStart()).toBe("");
  expect("  ".trimStart()).toBe("");
  expect("  hello world".trimStart()).toBe("hello world");
  expect("hello world  ".trimStart()).toBe("hello world  ");
});
