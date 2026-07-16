/*---
description: String legacy octal escape semantics
features: [string-literals]
---*/

test("sloppy strings cook legacy octal escapes", () => {
  expect("\1").toBe(String.fromCharCode(1));
  expect("\10").toBe(String.fromCharCode(8));
  expect("\377").toBe("\xff");
  expect("\400").toBe("\x20" + "0");
  expect("\08").toBe("\x00" + "8");
});
