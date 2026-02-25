describe("computed symbol getters on class instances", () => {
  test("Symbol.toStringTag getter", () => {
    class Foo {
      get [Symbol.toStringTag]() {
        return "Foo";
      }
    }
    const f = new Foo();
    expect(f[Symbol.toStringTag]).toBe("Foo");
  });

  test("Symbol.toStringTag integrates with Object.prototype.toString", () => {
    class MyClass {
      get [Symbol.toStringTag]() {
        return "MyClass";
      }
    }
    const obj = new MyClass();
    expect(Object.prototype.toString.call(obj)).toBe("[object MyClass]");
  });

  test("custom symbol getter", () => {
    const sym = Symbol("custom");
    class Bar {
      get [sym]() {
        return 42;
      }
    }
    const b = new Bar();
    expect(b[sym]).toBe(42);
  });

  test("computed string getter via bracket notation", () => {
    const key = "hello";
    class Baz {
      get [key]() {
        return "world";
      }
    }
    const b = new Baz();
    expect(b.hello).toBe("world");
  });

  test("symbol getter is inherited by instances", () => {
    const sym = Symbol("inherited");
    class Parent {
      get [sym]() {
        return "from parent";
      }
    }
    class Child extends Parent {}
    const c = new Child();
    expect(c[sym]).toBe("from parent");
  });

  test("symbol getter can be overridden in subclass", () => {
    const sym = Symbol("overridden");
    class Parent {
      get [sym]() {
        return "parent";
      }
    }
    class Child extends Parent {
      get [sym]() {
        return "child";
      }
    }
    expect(new Parent()[sym]).toBe("parent");
    expect(new Child()[sym]).toBe("child");
  });

  test("multiple symbol getters on same class", () => {
    const sym1 = Symbol("a");
    const sym2 = Symbol("b");
    class Multi {
      get [sym1]() { return 1; }
      get [sym2]() { return 2; }
    }
    const m = new Multi();
    expect(m[sym1]).toBe(1);
    expect(m[sym2]).toBe(2);
  });
});

describe("computed symbol setters on class instances", () => {
  test("custom symbol setter", () => {
    const sym = Symbol("setter");
    let captured;
    class Foo {
      set [sym](v) {
        captured = v;
      }
    }
    const f = new Foo();
    f[sym] = "hello";
    expect(captured).toBe("hello");
  });

  test("symbol getter and setter on different symbols", () => {
    const getSym = Symbol("get");
    const setSym = Symbol("set");
    let stored = 0;
    class Foo {
      get [getSym]() { return stored; }
      set [setSym](v) { stored = v; }
    }
    const f = new Foo();
    f[setSym] = 42;
    expect(f[getSym]).toBe(42);
  });

  test("symbol getter and setter on same symbol (merge)", () => {
    const sym = Symbol("accessor");
    let stored = 0;
    class Foo {
      get [sym]() { return stored; }
      set [sym](v) { stored = v; }
    }
    const f = new Foo();
    expect(f[sym]).toBe(0);
    f[sym] = 99;
    expect(f[sym]).toBe(99);
    f[sym] = 42;
    expect(f[sym]).toBe(42);
  });

  test("assigning to getter-only computed symbol throws TypeError", () => {
    const sym = Symbol("getterOnly");
    class Foo {
      get [sym]() { return 1; }
    }
    const f = new Foo();
    expect(() => { f[sym] = 2; }).toThrow(TypeError);
    expect(f[sym]).toBe(1);
  });
});

describe("computed symbol accessors on static members", () => {
  test("static computed symbol getter", () => {
    const sym = Symbol("static");
    class Foo {
      static get [sym]() {
        return "static value";
      }
    }
    expect(Foo[sym]).toBe("static value");
  });

  test("static computed symbol setter", () => {
    const sym = Symbol("staticSet");
    let captured;
    class Foo {
      static set [sym](v) {
        captured = v;
      }
    }
    Foo[sym] = "test";
    expect(captured).toBe("test");
  });

  test("static getter and setter on same symbol (merge)", () => {
    const sym = Symbol("staticAccessor");
    let stored = 0;
    class Foo {
      static get [sym]() { return stored; }
      static set [sym](v) { stored = v; }
    }
    expect(Foo[sym]).toBe(0);
    Foo[sym] = 100;
    expect(Foo[sym]).toBe(100);
    Foo[sym] = 200;
    expect(Foo[sym]).toBe(200);
  });

  test("assigning to static getter-only symbol throws TypeError", () => {
    const sym = Symbol("getterOnly");
    class Foo {
      static get [sym]() {
        return "read-only";
      }
    }
    expect(() => { Foo[sym] = "x"; }).toThrow(TypeError);
  });
});
