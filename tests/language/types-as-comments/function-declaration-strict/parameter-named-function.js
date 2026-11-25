/*---
description: Under strict types, a body function declaration named like a typed parameter is checked when it shares the parameter's binding (ES2026 §10.2.11 step 29), and has an untyped binding of its own when the parameter list has an expression (step 30)
features: [types-as-comments, strict-type-enforcement, compat-function]
---*/

describe("a function declaration named like a typed parameter", () => {

test("in an arrow function body, it shares the binding and is checked", () => {
  const m = (x: number) => { function x() {} return x + 1; };
  expect(() => m(1)).toThrow(TypeError);
});

test("in a class method body, it shares the binding and is checked", () => {
  class C { m(x: number) { function x() {} return typeof x; } }
  expect(() => new C().m(1)).toThrow(TypeError);
});

test("with an expression in the parameter list, it has a binding of its own", () => {
  class C { m(x: number, y = () => 0) { function x() {} return typeof x; } }
  expect(new C().m(1)).toBe("function");
});

test("with a closure over the parameter in the list, it has a binding of its own", () => {
  class C { m(x: number, y = () => x) { function x() {} return typeof x; } }
  expect(new C().m(1)).toBe("function");
});

test("an untyped parameter takes the function", () => {
  const m = (x, y = 0) => { function x() {} return typeof x; };
  expect(m(1)).toBe("function");
});

}); // describe
