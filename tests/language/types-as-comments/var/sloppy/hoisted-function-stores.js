/*---
description: A function declaration that shares a typed var's binding is a store into it, checked when the enclosing function starts
features: [compat-function, compat-var, strict-type-enforcement]
---*/

describe("function declarations sharing a typed var binding", () => {
  test("a function stored into a primitive-typed var throws on entry", () => {
    let started = false;
    function run() {
      started = true;
      var value: string = "text";
      function value() {}
      return value;
    }

    expect(run).toThrow(TypeError);
    expect(started).toBe(false);
  });

  test("a function stored into an object-typed var is accepted", () => {
    function run() {
      var value: object = {};
      function value() {}
      return typeof value;
    }

    expect(run()).toBe("object");
  });
});
