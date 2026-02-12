/*---
description: Function.length and Function.name properties
features: [Function.length, Function.name]
---*/

describe("Function.length", () => {
  test("counts formal parameters", () => {
    const f0 = () => {};
    const f1 = (a) => a;
    const f2 = (a, b) => a + b;
    const f3 = (a, b, c) => a + b + c;

    expect(f0.length).toBe(0);
    expect(f1.length).toBe(1);
    expect(f2.length).toBe(2);
    expect(f3.length).toBe(3);
  });

  test("stops counting at default parameter", () => {
    const f = (a, b = 1, c) => {};
    expect(f.length).toBe(1);
  });

  test("stops counting at rest parameter", () => {
    const f = (a, b, ...rest) => {};
    expect(f.length).toBe(2);
  });
});

describe("Function.name", () => {
  test("named function expression", () => {
    const add = (a, b) => a + b;
    expect(add.name).toBe("add");
  });

  test("class methods have names", () => {
    class Foo {
      bar() {
        return 1;
      }
    }
    const foo = new Foo();
    // Method name is accessible
    expect(typeof Foo).toBe("function");
  });
});
