/*---
description: Computed property names in class method definitions
features: [computed-property-names, class]
---*/

describe("computed method names in classes", () => {
  test("computed string method name on prototype", () => {
    class C {
      ["foo"]() { return 42; }
    }
    const c = new C();
    expect(c.foo()).toBe(42);
  });

  test("computed string method name with static", () => {
    class C {
      static ["bar"]() { return "baz"; }
    }
    expect(C.bar()).toBe("baz");
  });

  test("computed numeric method name", () => {
    class C {
      static [1]() { return "one"; }
      static [2]() { return "two"; }
    }
    expect(C[1]()).toBe("one");
    expect(C[2]()).toBe("two");
  });

  test("computed method with expression", () => {
    const prefix = "get";
    const suffix = "Value";
    class C {
      [prefix + suffix]() { return 99; }
    }
    const c = new C();
    expect(c.getValue()).toBe(99);
  });

  test("computed and regular methods coexist", () => {
    class C {
      static a() { return "A"; }
      static ["b"]() { return "B"; }
      static c() { return "C"; }
      static ["d"]() { return "D"; }
    }
    expect(C.a()).toBe("A");
    expect(C.b()).toBe("B");
    expect(C.c()).toBe("C");
    expect(C.d()).toBe("D");
  });

  test("computed static constructor method", () => {
    class C {
      static ["constructor"]() { return "static-ctor"; }
    }
    expect(C.constructor()).toBe("static-ctor");
  });

  test("computed Symbol method name", () => {
    const sym = Symbol("myMethod");
    class C {
      [sym]() { return "symbol-value"; }
    }
    const c = new C();
    expect(c[sym]()).toBe("symbol-value");
  });
});
