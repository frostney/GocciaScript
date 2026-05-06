/*---
description: Find* scope walks stop at ordinary function boundaries
features: [class-declaration, new-target, super]
---*/

describe("new.target function boundary", () => {
  test("arrow inherits new.target from constructor", () => {
    class Foo {
      constructor() {
        const arrow = () => new.target;
        this.result = arrow();
      }
    }
    const f = new Foo();
    expect(f.result).toBe(Foo);
  });

  test("ordinary method does not inherit new.target", () => {
    class Foo {
      constructor() {
        const obj = { method() { return new.target; } };
        this.result = obj.method();
      }
    }
    const f = new Foo();
    expect(f.result).toBe(undefined);
  });

  test("deeply nested arrows inherit new.target", () => {
    class Foo {
      constructor() {
        const a1 = () => {
          const a2 = () => new.target;
          return a2();
        };
        this.result = a1();
      }
    }
    const f = new Foo();
    expect(f.result).toBe(Foo);
  });

  test("ordinary function inside arrow blocks new.target", () => {
    class Foo {
      constructor() {
        const arrow = () => {
          const obj = { f() { return new.target; } };
          return obj.f();
        };
        this.result = arrow();
      }
    }
    const f = new Foo();
    expect(f.result).toBe(undefined);
  });
});

describe("super function boundary", () => {
  class Base {
    hello() { return "base"; }
  }

  test("arrow inherits super from class method", () => {
    class Derived extends Base {
      test() {
        const arrow = () => super.hello();
        return arrow();
      }
    }
    expect(new Derived().test()).toBe("base");
  });

  test("ordinary method does not inherit super from class method", () => {
    class Derived extends Base {
      test() {
        const obj = { inner() { return super.hello(); } };
        expect(() => obj.inner()).toThrow();
      }
    }
    new Derived().test();
  });

  test("deeply nested arrows inherit super", () => {
    class Derived extends Base {
      test() {
        const a1 = () => {
          const a2 = () => super.hello();
          return a2();
        };
        return a1();
      }
    }
    expect(new Derived().test()).toBe("base");
  });

  test("ordinary function inside arrow blocks super", () => {
    class Derived extends Base {
      test() {
        const arrow = () => {
          const obj = { f() { return super.hello(); } };
          return obj.f();
        };
        expect(() => arrow()).toThrow();
      }
    }
    new Derived().test();
  });
});
