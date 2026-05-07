/*---
description: Reflect.construct honors explicit object returns from class constructors per ES2026 §10.2.2 step 11
features: [Reflect]
---*/

describe("Reflect.construct with class constructor explicit object return", () => {
  test("base class explicit object return replaces the receiver", () => {
    class Foo {
      constructor() {
        return { explicit: true };
      }
    }
    const obj = Reflect.construct(Foo, []);
    expect(obj.explicit).toBe(true);
    expect(obj instanceof Foo).toBe(false);
  });

  test("base class explicit object return matches new operator behavior", () => {
    class Foo {
      constructor() {
        return { replaced: true };
      }
    }
    const viaNew = new Foo();
    const viaReflect = Reflect.construct(Foo, []);
    expect(viaNew.replaced).toBe(true);
    expect(viaReflect.replaced).toBe(true);
  });

  test("base class primitive return is ignored — receiver wins", () => {
    class Foo {
      constructor() {
        this.kept = true;
        return 42;
      }
    }
    const obj = Reflect.construct(Foo, []);
    expect(obj.kept).toBe(true);
    expect(obj instanceof Foo).toBe(true);
  });

  test("derived class explicit object return replaces the receiver", () => {
    class Base {}
    class Derived extends Base {
      constructor() {
        super();
        return { derived: true };
      }
    }
    const obj = Reflect.construct(Derived, []);
    expect(obj.derived).toBe(true);
    expect(obj instanceof Derived).toBe(false);
  });

  test("derived class primitive return throws TypeError", () => {
    class Base {}
    class Derived extends Base {
      constructor() {
        super();
        return 7;
      }
    }
    expect(() => Reflect.construct(Derived, [])).toThrow(TypeError);
  });

  test("explicit object return prototype is not patched by newTarget", () => {
    class Foo {
      constructor() {
        return { userObj: true };
      }
    }
    class NT {}
    const obj = Reflect.construct(Foo, [], NT);
    expect(obj.userObj).toBe(true);
    expect(Object.getPrototypeOf(obj)).toBe(Object.prototype);
    expect(Object.getPrototypeOf(obj)).not.toBe(NT.prototype);
  });

  test("super() replacement object is returned when constructor returns undefined", () => {
    class Base {
      constructor() {
        return { fromBase: true };
      }
    }
    class Derived extends Base {
      constructor() {
        super();
      }
    }
    const obj = Reflect.construct(Derived, []);
    expect(obj.fromBase).toBe(true);
  });

  test("constructor arguments are forwarded correctly", () => {
    class Foo {
      constructor(x, y) {
        return { sum: x + y };
      }
    }
    const obj = Reflect.construct(Foo, [3, 4]);
    expect(obj.sum).toBe(7);
  });
});

describe("Reflect.construct with class constructor via proxy fallback", () => {
  test("proxy without construct trap honors explicit object return from class", () => {
    class Foo {
      constructor() {
        return { proxied: true };
      }
    }
    const proxy = new Proxy(Foo, {});
    const obj = Reflect.construct(proxy, []);
    expect(obj.proxied).toBe(true);
    expect(obj instanceof Foo).toBe(false);
  });

  test("proxy fallback derived class primitive return throws TypeError", () => {
    class Base {}
    class Derived extends Base {
      constructor() {
        super();
        return 7;
      }
    }
    const proxy = new Proxy(Derived, {});
    expect(() => Reflect.construct(proxy, [])).toThrow(TypeError);
  });
});
