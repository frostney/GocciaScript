/*---
description: With an expression in the parameter list, a body var or function named arguments is a binding of the body, so closures in the parameter list keep the arguments object; the arguments object is unmapped (ES2026 §10.2.11 FunctionDeclarationInstantiation)
features: [compat-function, compat-var, compat-arguments-object, compat-non-strict-mode, default-parameters]
---*/

describe("arguments when the parameter list has expressions", () => {
  test("a body var named arguments leaves the default closure's arguments object alone", () => {
    function g(a, f = () => arguments) {
      var arguments = 5;
      return [arguments, f().length];
    }
    expect(g(1, undefined, 3)).toEqual([5, 3]);
  });

  test("a body function named arguments leaves the default closure's arguments object alone", () => {
    function g(a = 0, f = () => arguments) {
      function arguments() {}
      return [typeof arguments, typeof f()];
    }
    expect(g()).toEqual(["function", "object"]);
  });

  test("an arrow function has no arguments of its own, so its default closure sees the enclosing function's", () => {
    function outer() {
      return ((x = 0, f = () => arguments) => {
        var arguments = 3;
        return [arguments, f().length];
      })();
    }
    expect(outer(1, 2)).toEqual([3, 2]);
  });

  test("a body var named like a parameter is not mapped to the unmapped arguments object", () => {
    function g(a, b = 1) {
      var a = 4;
      return [a, arguments[0]];
    }
    expect(g(1)).toEqual([4, 1]);
  });

  test("a strict class method with a body var named like a parameter", () => {
    class C {
      m(a, f = () => a) {
        var a = 5;
        return [a, f(), arguments.length, arguments[0]];
      }
    }
    expect(new C().m(1)).toEqual([5, 1, 1, 1]);
  });

  test("a sloppy function with a body var named like a parameter", () => {
    function g(a, f = () => a) {
      var a = 5;
      return [a, f(), arguments[0]];
    }
    expect(g(1)).toEqual([5, 1, 1]);
  });

  test("without a parameter expression a sloppy function maps the parameter and its var", () => {
    function g(a) {
      var a = 4;
      return arguments[0];
    }
    expect(g(1)).toBe(4);
  });
});
