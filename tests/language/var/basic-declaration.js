/*---
description: Basic var declaration with initializer
features: [compat-var]
---*/

test("var declaration with initializer", () => {
  var x = 1;
  expect(x).toBe(1);
});

test("var declaration with string value", () => {
  var name = "hello";
  expect(name).toBe("hello");
});

test("var declaration without initializer defaults to undefined", () => {
  var x;
  expect(x).toBeUndefined();
});
