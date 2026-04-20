/*---
description: var allows redeclaration without errors
features: [compat-var]
---*/

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
