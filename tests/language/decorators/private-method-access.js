/*---
description: A private method decorator's context.access reads the method through the class's brand
features: [decorators]
---*/

// A replacement that keeps the receiver: a method, not an arrow function.
const addHundred = (value) => ({
  wrapped(...args) {
    return value.call(this, ...args) + 100;
  },
}).wrapped;

describe("private instance method decorator context", () => {
  test("the method passed in is named after the private name", () => {
    let original;
    class A {
      @((value) => { original = value; }) #m() {}
    }

    expect(original.name).toBe("#m");
  });

  test("context.access has get and has, and no set", () => {
    let context;
    class A {
      @((value, ctx) => { context = ctx; }) #m() {}
    }

    expect(context.static).toBe(false);
    expect(context.private).toBe(true);
    expect(typeof context.addInitializer).toBe("function");
    expect(typeof context.access.get).toBe("function");
    expect(typeof context.access.has).toBe("function");
    expect("set" in context.access).toBe(false);
  });

  test("context.access.get returns the replacement", () => {
    let context;
    let original;
    let replacement;
    const dec = (value, ctx) => {
      context = ctx;
      original = value;
      replacement = addHundred(value);
      return replacement;
    };

    class A {
      @dec #m() { return 1; }
    }

    const a = new A();
    expect(context.access.get(a)).toBe(replacement);
    expect(context.access.get(a)).not.toBe(original);
    expect(context.access.get(a).call(a)).toBe(101);
  });

  test("context.access.get returns the method a decorator kept", () => {
    let context;
    let original;
    class A {
      @((value, ctx) => { context = ctx; original = value; }) #m() {}
    }

    expect(typeof original).toBe("function");
    expect(context.access.get(new A())).toBe(original);
  });

  test("context.access checks the brand", () => {
    let contextA;
    let contextB;
    class A {
      @((value, ctx) => { contextA = ctx; }) #m() { return "A"; }
    }
    class B {
      @((value, ctx) => { contextB = ctx; }) #m() { return "B"; }
    }

    const a = new A();
    const b = new B();
    expect(contextA.access.has(a)).toBe(true);
    expect(contextA.access.has(b)).toBe(false);
    expect(contextA.access.has({})).toBe(false);
    expect(contextB.access.get(b)()).toBe("B");
    expect(() => contextA.access.get(b)).toThrow(TypeError);
    expect(() => contextA.access.get({})).toThrow(TypeError);
  });

  test("context.access throws a TypeError for a primitive", () => {
    let context;
    class A {
      @((value, ctx) => { context = ctx; }) #m() {}
    }

    expect(() => context.access.get(1)).toThrow(TypeError);
    expect(() => context.access.has(1)).toThrow(TypeError);
    expect(() => context.access.has(undefined)).toThrow(TypeError);
  });

  test("context.access sees an object a base constructor returned", () => {
    let context;
    class Base {
      constructor(o) {
        return o;
      }
    }
    class D extends Base {
      @((value, ctx) => {
        context = ctx;
        return addHundred(value);
      }) #m() { return 1; }
      static callOn(o) { return o.#m(); }
    }

    const o = {};
    new D(o);
    expect(D.callOn(o)).toBe(101);
    expect(context.access.has(o)).toBe(true);
    expect(context.access.get(o).call(o)).toBe(101);
  });

  test("a decorator that returns a non-callable value throws a TypeError", () => {
    expect(() => {
      class A {
        @(() => 42) #m() {}
      }
    }).toThrow(TypeError);
  });
});

describe("static private method decorator context", () => {
  test("context.access reads the method from the class only", () => {
    let context;
    class A {
      @((value, ctx) => { context = ctx; }) static #m() { return 1; }
    }
    class B extends A {}

    expect(context.access.has(A)).toBe(true);
    expect(context.access.has(B)).toBe(false);
    expect(context.access.has({})).toBe(false);
    expect(context.access.get(A)()).toBe(1);
    expect(() => context.access.get(B)).toThrow(TypeError);
  });
});
