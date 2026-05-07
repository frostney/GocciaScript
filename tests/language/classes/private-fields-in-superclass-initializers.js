/*---
description: Superclass arrow field initializers resolve private fields against the declaring class
features: [private-fields, arrow-functions, class-inheritance]
---*/

describe("Superclass private fields in arrow field initializers", () => {
  test("superclass arrow field accesses own private field", () => {
    class Base {
      #x = 10;
      fn = () => this.#x;
    }
    class Derived extends Base {
      y = 20;
    }
    const d = new Derived();
    expect(d.fn()).toBe(10);
    expect(d.y).toBe(20);
  });

  test("deep chain: each class resolves own private fields", () => {
    class A {
      #a = 1;
      getA = () => this.#a;
    }
    class B extends A {
      #b = 2;
      getB = () => this.#b;
    }
    class C extends B {
      #c = 3;
      getC = () => this.#c;
    }
    const c = new C();
    expect(c.getA()).toBe(1);
    expect(c.getB()).toBe(2);
    expect(c.getC()).toBe(3);
  });
});
