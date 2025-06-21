/*---
description: String.prototype.repeat works correctly
features: [String.prototype.repeat]
---*/

test("String.prototype.repeat repeats strings", () => {
  expect("abc".repeat(3)).toBe("abcabcabc");
  expect("x".repeat(0)).toBe("");
  expect("test".repeat(1)).toBe("test");
  expect("".repeat(1)).toBe("");
});
