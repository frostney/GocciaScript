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

test("braced unicode escapes accept padded code points", () => {
  expect("\u{00000000000001000}".charCodeAt(0)).toBe(0x1000);
  expect("\u{00000000000000000}".charCodeAt(0)).toBe(0x0000);
  expect("\u{0000000000010FFFF}").toBe(String.fromCodePoint(0x10ffff));
});

test("braced unicode escapes can produce lone surrogate code units", () => {
  expect("\u{D800}".length).toBe(1);
  expect("\u{D800}".charCodeAt(0)).toBe(0xd800);
  expect("\u{DFFF}".charCodeAt(0)).toBe(0xdfff);
});

test("template braced unicode escapes accept padded code points", () => {
  expect(`\u{00000000000001000}`.charCodeAt(0)).toBe(0x1000);
  expect(`\u{0000000000010FFFF}`).toBe(String.fromCodePoint(0x10ffff));
  expect(`\u{D800}`.charCodeAt(0)).toBe(0xd800);
});

test("template hex escapes above ASCII are cooked as UTF-8", () => {
  expect(`\xA9`).toBe(String.fromCharCode(0x00a9));
});
