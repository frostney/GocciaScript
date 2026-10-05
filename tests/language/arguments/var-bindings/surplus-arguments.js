/*---
description: Surplus arguments reach the arguments object but not the body's vars
features: [compat-arguments-object, compat-function, compat-non-strict-mode, compat-var]
---*/

// ES2026 §10.2.11 FunctionDeclarationInstantiation: the arguments object holds
// every argument (step 22), and every var the body declares starts as
// undefined (step 29.c.i.3).

test("unmapped vars stay undefined while arguments holds every argument", () => {
  function fn(a) {
    var x, y;
    return [x, y, arguments.length, arguments[1], arguments[2]];
  }
  expect(fn(1, 2, 3)).toEqual([undefined, undefined, 3, 2, 3]);
});

test("a mapped parameter write leaves the vars undefined", () => {
  function fn(a) {
    var x;
    arguments[0] = 9;
    return [a, x, arguments[1]];
  }
  expect(fn(1, 2)).toEqual([9, undefined, 2]);
});

test("var arguments keeps the arguments object", () => {
  function fn() {
    var arguments;
    return [typeof arguments, arguments.length];
  }
  expect(fn(1, 2)).toEqual(["object", 2]);
});
