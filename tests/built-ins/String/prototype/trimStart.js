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

test("String.prototype.trimStart removes ECMAScript Unicode whitespace", () => {
  const whitespace = "\u00A0\u1680\u2000\u2028\u2029\u202F\u205F\u3000\uFEFF";
  expect((whitespace + "hello" + whitespace).trimStart()).toBe(
    "hello" + whitespace
  );
});

test("String.prototype.trimStart coerces non-string receivers", () => {
  expect(String.prototype.trimStart.call(42)).toBe("42");
  expect(String.prototype.trimStart.call(true)).toBe("true");
});
