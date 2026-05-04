/*---
description: Basic var declaration with initializer
features: [compat-var]
---*/

var async = 1;
const __gocciaVarAsyncInitial = async;
async = 2;
const __gocciaVarAsyncUpdated = async;

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

test("var can bind async as a contextual identifier", () => {
  expect(__gocciaVarAsyncInitial).toBe(1);
  expect(__gocciaVarAsyncUpdated).toBe(2);
  expect(globalThis.async).toBe(2);
});
