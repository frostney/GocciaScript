/*---
description: The Annex B var binding of a block function starts as undefined, whatever the call passes
features: [compat-function, compat-non-strict-mode, compat-var]
---*/

// ES2026 B.3.2.1 (FunctionDeclarationInstantiation web-compat step): a block
// function's var binding is initialized to undefined and gets the function
// only when its declaration is evaluated.

test("block function var binding before its block runs, with surplus arguments", () => {
  function outer(a) {
    var before = typeof f;
    {
      function f() {}
    }
    return [before, typeof f];
  }
  expect(outer(1, "surplus", 3)).toEqual(["undefined", "function"]);
});

test("block function var binding after destructured parameters", () => {
  function outer({ a }, [b]) {
    var before = typeof f;
    {
      function f() {}
    }
    return [a, b, before];
  }
  expect(outer({ a: "x" }, ["y"])).toEqual(["x", "y", "undefined"]);
});
