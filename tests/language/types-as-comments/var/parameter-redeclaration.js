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

test("a rejected initializer leaves the parameter holding its last value", () => {
  class C {
    m(x: number) {
      const seen = [];
      for (const v of [1, "s", 2]) {
        try { var x = v; } catch (e) { seen.push(e.name); }
        seen.push(x + 1);
      }
      return seen.join(",");
    }
  }
  expect(new C().m(1)).toBe("2,TypeError,2,3");
});

test("a redeclaration keeps its own annotation's check", () => {
  const m = () => { var x: number = 1; var x: string = 2; return x; };
  expect(() => m()).toThrow(TypeError);
});

test("a rejected for-of var head closes the iterator", () => {
  let closed = 0;
  const iterable = {
    [Symbol.iterator]() {
      return { next: () => ({ value: "s", done: false }), return() { closed++; return {}; } };
    },
  };
  const m = (x: number) => { for (var x of iterable) {} };
  expect(() => m(1)).toThrow(TypeError);
  expect(closed).toBe(1);
});

test("a rejected for-await var head closes the iterator", async () => {
  let closed = 0;
  const iterable = {
    [Symbol.asyncIterator]() {
      return { next: async () => ({ value: "s", done: false }), return: async () => { closed++; return {}; } };
    },
  };
  const m = async (x: number) => { for await (var x of iterable) {} };
  await expect(m(1)).rejects.toThrow(TypeError);
  expect(closed).toBe(1);
});

test("with an expression in the parameter list, the var is a binding of its own", () => {
  const m = (x: number, y = 0) => { var x = "s"; return x; };
  expect(m(1)).toBe("s");
});

}); // describe
