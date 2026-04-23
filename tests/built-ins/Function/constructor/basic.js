/*---
description: Function constructor basic usage
features: [Function, unsafe-function-constructor]
---*/

describe("Function constructor", () => {
  test("no arguments creates empty function", () => {
    const f = new Function();
    expect(typeof f).toBe("function");
    expect(f()).toBe(undefined);
  });

  test("body-only argument", () => {
    const f = new Function("return 42");
    expect(f()).toBe(42);
  });

  test("single parameter and body", () => {
    const f = new Function("a", "return a");
    expect(f(5)).toBe(5);
  });

  test("two parameters and body", () => {
    const f = new Function("a", "b", "return a + b");
    expect(f(2, 3)).toBe(5);
  });

  test("multiple parameters and body", () => {
    const f = new Function("a", "b", "c", "return a + b + c");
    expect(f(1, 2, 3)).toBe(6);
  });

  test("comma-separated params in single string", () => {
    const f = new Function("a, b", "return a + b");
    expect(f(10, 20)).toBe(30);
  });

  test("Function() without new is equivalent", () => {
    const f = Function("a", "return a * 2");
    expect(f(5)).toBe(10);
  });

  test("body with multiple statements", () => {
    const f = new Function("a", "const x = a * 2; return x + 1");
    expect(f(10)).toBe(21);
  });
});
