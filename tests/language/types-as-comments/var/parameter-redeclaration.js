/*---
description: Under strict types, a var that redeclares a typed parameter in a body whose parameter list has no expression stores into the parameter's binding, so each store is checked against the parameter's type
features: [types-as-comments, strict-type-enforcement, compat-var]
---*/

describe("a var redeclaring a typed parameter", () => {

test("its initializer is checked against the parameter's type", () => {
  class C {
    m(x: number) {
      const read = () => x + 1;
      var x = "s";
      return read();
    }
  }
  expect(() => new C().m(1)).toThrow(TypeError);
});

test("a matching initializer is stored", () => {
  class C {
    m(x: number) {
      const read = () => x + 1;
      var x = 2;
      return read();
    }
  }
  expect(new C().m(1)).toBe(3);
});

test("a destructuring var is checked", () => {
  const m = (x: number) => { var [x] = ["s"]; return x + 1; };
  expect(() => m(1)).toThrow(TypeError);
});

test("a for-of var head is checked", () => {
  const m = (x: number) => { for (var x of ["s"]) {} return x + 1; };
  expect(() => m(1)).toThrow(TypeError);
});

test("with an expression in the parameter list, the var is a binding of its own", () => {
  const m = (x: number, y = 0) => { var x = "s"; return x; };
  expect(m(1)).toBe("s");
});

}); // describe
