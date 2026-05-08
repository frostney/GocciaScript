/*---
description: Function constructor creates non-strict functions for this-binding (ES2026 15.2.2.4)
features: [Function, unsafe-function-constructor]
---*/

describe("Function constructor this-binding", () => {
  test("Function('return this')() returns globalThis", () => {
    const f = Function("return this");
    expect(f()).toBe(globalThis);
  });

  test("new Function('return this')() returns globalThis", () => {
    const f = new Function("return this");
    expect(f()).toBe(globalThis);
  });

  test("Function constructor this is globalThis even in module-shaped code", () => {
    const getThis = Function("return this");
    const result = getThis();
    expect(result).toBe(globalThis);
    expect(result).not.toBe(undefined);
  });

  test("Function.call(undefined) coerces to globalThis", () => {
    const f = Function("return this");
    expect(f.call(undefined)).toBe(globalThis);
  });

  test("Function.call(null) coerces to globalThis", () => {
    const f = Function("return this");
    expect(f.call(null)).toBe(globalThis);
  });

  test("Function.apply(undefined) coerces to globalThis", () => {
    const f = Function("return this");
    expect(f.apply(undefined)).toBe(globalThis);
  });

  test("Function.call with explicit receiver preserves it", () => {
    const obj = { x: 42 };
    const f = Function("return this.x");
    expect(f.call(obj)).toBe(42);
  });

  test("bound Function constructor preserves bound this", () => {
    const obj = { value: 99 };
    const f = Function("return this.value");
    const bound = f.bind(obj);
    expect(bound()).toBe(99);
  });
});
