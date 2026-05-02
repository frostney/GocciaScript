/*---
description: var allows redeclaration without errors
features: [compat-var]
---*/

var __gocciaGlobalVarUndefinedInitializer = 19;
var __gocciaGlobalVarUndefinedInitializer = undefined;

test("var redeclaration does not throw", () => {
  var x = 1;
  var x = 2;
  expect(x).toBe(2);
});

test("var redeclaration with different values", () => {
  var a = "first";
  var a = "second";
  var a = "third";
  expect(a).toBe("third");
});

test("var redeclaration without initializer preserves value", () => {
  var x = 42;
  var x;
  expect(x).toBe(42);
});

test("top-level var undefined initializer updates global-backed binding", () => {
  expect(__gocciaGlobalVarUndefinedInitializer).toBeUndefined();
  expect(globalThis.__gocciaGlobalVarUndefinedInitializer).toBeUndefined();
});
