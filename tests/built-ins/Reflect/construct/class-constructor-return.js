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
    const viaNew = new Foo();
    const viaReflect = Reflect.construct(Foo, []);
    expect(viaReflect.explicit).toBe(true);
    expect(viaReflect instanceof Foo).toBe(false);
    expect(viaNew.explicit).toBe(viaReflect.explicit);
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

  test("base class returning null is ignored — receiver wins", () => {
    class Foo {
      constructor() {
        this.kept = true;
        return null;
      }
    }
    const obj = Reflect.construct(Foo, []);
    expect(obj.kept).toBe(true);
    expect(obj instanceof Foo).toBe(true);
  });

  test("derived class returning null throws TypeError", () => {
    class Base {}
    class Derived extends Base {
      constructor() {
        super();
        return null;
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

  test("super() replacement with separate newTarget returns replacement unchanged", () => {
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
    class NT {}
    const obj = Reflect.construct(Derived, [], NT);
    expect(obj.fromBase).toBe(true);
    expect(Object.getPrototypeOf(obj)).toBe(Object.prototype);
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
