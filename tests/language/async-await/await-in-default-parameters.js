/*---
description: Await in async function parameter defaults is rejected (ES2026 §15.8.2)
features: [async-await, Function, unsafe-function-constructor]
---*/

describe("await in async function parameter defaults", () => {
  test("async arrow with default parameter works normally", async () => {
    const fn = async (x = 42) => x;
    expect(await fn()).toBe(42);
    expect(await fn(10)).toBe(10);
  });

  test("async arrow body can still use await", async () => {
    const fn = async (x = 42) => {
      return await Promise.resolve(x);
    };
    expect(await fn()).toBe(42);
  });

  test("await without an operand in async arrow body throws", () => {
    expect(() => new Function("return async () => { await; }")).toThrow();
  });

  test("await in async arrow parameter default throws", () => {
    expect(() => new Function("return async (x = await 1) => x")).toThrow();
  });

  test("await in async function expression parameter default throws", () => {
    expect(() => new Function("return async function(x = await 1) {}")).toThrow();
  });

  test("await in async object method parameter default throws", () => {
    expect(() => new Function("return { async m(x = await 1) {} }")).toThrow();
  });

  test("await in async class method parameter default throws", () => {
    expect(() => new Function("class C { async m(x = await 1) {} }")).toThrow();
  });
});
