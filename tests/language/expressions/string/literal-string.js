/*---
description: Literal string support
features: [literal-strings]
---*/

test("literal string with double quotes", () => {
  const str = "hello world";
  expect(str).toBe("hello world");
});
