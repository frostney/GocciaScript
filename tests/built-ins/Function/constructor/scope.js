/*---
description: Function constructor - scope behavior
features: [Function, unsafe-function-constructor]
---*/

describe("Function constructor scope", () => {
  test("does not capture lexical scope", () => {
    const x = 42;
    const f = new Function("return typeof x");
    // Per ES spec, Function constructor creates functions in global scope
    // so local 'x' is not visible
    expect(f()).toBe("undefined");
  });

  test("can access global built-ins", () => {
    const f = new Function("return typeof Math");
    expect(f()).toBe("object");
  });

  test("receives its own this", () => {
    const f = new Function("return this");
    const obj = { method: f };
    expect(obj.method()).toBe(obj);
  });
});
