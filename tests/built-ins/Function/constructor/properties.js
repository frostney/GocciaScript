/*---
description: Function constructor - function properties
features: [Function, unsafe-function-constructor]
---*/

describe("Function constructor properties", () => {
  test("length reflects parameter count", () => {
    expect(new Function().length).toBe(0);
    expect(new Function("return 1").length).toBe(0);
    expect(new Function("a", "return a").length).toBe(1);
    expect(new Function("a", "b", "return a + b").length).toBe(2);
  });

  test("name is 'anonymous'", () => {
    const f = new Function("return 1");
    expect(f.name).toBe("anonymous");
  });

  test("instanceof Function", () => {
    const f = new Function("return 1");
    expect(f instanceof Function).toBe(true);
  });

  test("typeof is 'function'", () => {
    const f = new Function("return 1");
    expect(typeof f).toBe("function");
  });
});
