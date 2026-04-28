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

test("String.prototype.trimEnd removes ECMAScript Unicode whitespace", () => {
  const whitespace = "\u00A0\u1680\u2000\u2028\u2029\u202F\u205F\u3000\uFEFF";
  expect((whitespace + "hello" + whitespace).trimEnd()).toBe(
    whitespace + "hello"
  );
});

test("String.prototype.trimEnd coerces non-string receivers", () => {
  expect(String.prototype.trimEnd.call(42)).toBe("42");
  expect(String.prototype.trimEnd.call(true)).toBe("true");
});
