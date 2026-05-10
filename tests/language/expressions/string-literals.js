/*---
description: String literal escape semantics
features: [string-literals]
---*/

test("single-character string escapes are cooked correctly", () => {
  expect("\b").toBe(String.fromCharCode(0x0008));
  expect("\f").toBe(String.fromCharCode(0x000c));
  expect("\v").toBe(String.fromCharCode(0x000b));
});

test("line continuations are omitted from string literal values", () => {
  expect("left\
right").toBe("leftright");
  expect('left\
right').toBe("leftright");
});

test("LS and PS are valid in string literals (ES2019)", () => {
  expect("hello world").toBe("hello" + String.fromCharCode(0x2028) + "world");
  expect('hello world').toBe("hello" + String.fromCharCode(0x2029) + "world");
});
