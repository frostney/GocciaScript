/*---
description: With an expression in the parameter list, the var binding Annex B.3.2.1 creates for a block-level function starts as undefined in the body's var environment (ES2026 §10.2.11 FunctionDeclarationInstantiation)
features: [compat-function, compat-var, compat-non-strict-mode, default-parameters]
---*/

describe("block-level functions when the parameter list has expressions", () => {
  test("the var binding starts as undefined and takes the function when the block runs", () => {
    function g(a = 0) {
      var before = typeof h;
      {
        function h() {}
      }
      return [before, typeof h];
    }
    expect(g(0, 1, 2)).toEqual(["undefined", "function"]);
  });

  test("a default closure keeps the parameter", () => {
    function g(a, f = () => a) {
      var before = typeof h;
      {
        function h() {}
      }
      return [before, typeof h, f()];
    }
    expect(g(5, undefined, 3)).toEqual(["undefined", "function", 5]);
  });
});
