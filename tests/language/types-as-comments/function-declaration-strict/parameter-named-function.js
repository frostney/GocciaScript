/*---
description: Under strict types, a body function declaration named like a typed parameter is checked as a store into the parameter, so it throws a TypeError instead of reaching code that trusts the parameter's type
features: [types-as-comments, strict-type-enforcement, compat-function]
---*/

describe("a function declaration named like a typed parameter", () => {

test("in an arrow function body", () => {
  const m = (x: number) => { function x() {} return x + 1; };
  expect(() => m(1)).toThrow(TypeError);
});

test("in a class method whose parameters have an expression", () => {
  class C { m(x: number, y = 0) { function x() {} return x + 1; } }
  expect(() => new C().m(1)).toThrow(TypeError);
});

test("an untyped parameter takes the function", () => {
  const m = (x, y = 0) => { function x() {} return typeof x; };
  expect(m(1)).toBe("function");
});

}); // describe
