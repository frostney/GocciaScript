/*---
description: Object.freeze and Object.isFrozen
features: [Object.freeze, Object.isFrozen]
---*/

describe("Object.freeze", () => {
  test("freezing an object prevents modification", () => {
    const obj = { a: 1, b: 2 };
    Object.freeze(obj);

    let threw = false;
    try {
      obj.a = 99;
    } catch (e) {
      threw = true;
    }
    expect(threw).toBe(true);
    expect(obj.a).toBe(1);
  });

  test("freeze returns the same object", () => {
    const obj = { x: 1 };
    const frozen = Object.freeze(obj);
    expect(frozen).toBe(obj);
  });

  test("Object.isFrozen returns true for frozen objects", () => {
    const obj = { a: 1 };
    expect(Object.isFrozen(obj)).toBe(false);
    Object.freeze(obj);
    expect(Object.isFrozen(obj)).toBe(true);
  });

  test("non-objects are considered frozen", () => {
    expect(Object.isFrozen(42)).toBe(true);
    expect(Object.isFrozen("hello")).toBe(true);
    expect(Object.isFrozen(true)).toBe(true);
  });

  test("freezing a non-object returns it as-is", () => {
    expect(Object.freeze(42)).toBe(42);
    expect(Object.freeze("hello")).toBe("hello");
  });

  test("frozen objects still allow reads", () => {
    const obj = { x: 10, y: 20 };
    Object.freeze(obj);
    expect(obj.x).toBe(10);
    expect(obj.y).toBe(20);
    expect(Object.keys(obj)).toEqual(["x", "y"]);
  });

  test("frozen properties are non-writable", () => {
    const obj = { a: 1, b: "hello" };
    Object.freeze(obj);

    const desc = Object.getOwnPropertyDescriptor(obj, "a");
    expect(desc.writable).toBe(false);
    expect(desc.configurable).toBe(false);
    expect(desc.value).toBe(1);
  });

  test("frozen properties retain their enumerable flag", () => {
    const obj = {};
    Object.defineProperty(obj, "hidden", {
      value: 42,
      enumerable: false,
      writable: true,
      configurable: true,
    });
    Object.defineProperty(obj, "visible", {
      value: 99,
      enumerable: true,
      writable: true,
      configurable: true,
    });

    Object.freeze(obj);

    const hiddenDesc = Object.getOwnPropertyDescriptor(obj, "hidden");
    expect(hiddenDesc.writable).toBe(false);
    expect(hiddenDesc.configurable).toBe(false);
    expect(hiddenDesc.enumerable).toBe(false);

    const visibleDesc = Object.getOwnPropertyDescriptor(obj, "visible");
    expect(visibleDesc.writable).toBe(false);
    expect(visibleDesc.configurable).toBe(false);
    expect(visibleDesc.enumerable).toBe(true);
  });

  test("cannot add new properties to a frozen object", () => {
    const obj = { a: 1 };
    Object.freeze(obj);

    let threw = false;
    try {
      obj.newProp = "nope";
    } catch (e) {
      threw = true;
    }
    expect(threw).toBe(true);
    expect(obj.newProp).toBe(undefined);
  });

  test("multiple properties all become non-writable", () => {
    const obj = { x: 1, y: 2, z: 3 };
    Object.freeze(obj);

    const keys = Object.keys(obj);
    keys.forEach((key) => {
      const desc = Object.getOwnPropertyDescriptor(obj, key);
      expect(desc.writable).toBe(false);
      expect(desc.configurable).toBe(false);
    });
  });

  test("freezing arrays locks dense elements and length while preserving reads", () => {
    const arr = ["a", "b"];
    Object.freeze(arr);

    const first = Object.getOwnPropertyDescriptor(arr, "0");
    const length = Object.getOwnPropertyDescriptor(arr, "length");
    expect(first.enumerable).toBe(true);
    expect(first.writable).toBe(false);
    expect(first.configurable).toBe(false);
    expect(length.writable).toBe(false);
    expect([...arr]).toEqual(["a", "b"]);

    try {
      arr[0] = "changed";
    } catch (error) {}
    try {
      delete arr[0];
    } catch (error) {}
    expect(arr[0]).toBe("a");
  });

  test("freezing String objects preserves virtual indices and length", () => {
    const str = new String("abc");
    str.foo = 10;

    Object.freeze(str);

    const index = Object.getOwnPropertyDescriptor(str, "0");
    const length = Object.getOwnPropertyDescriptor(str, "length");
    const foo = Object.getOwnPropertyDescriptor(str, "foo");

    expect(Object.isFrozen(str)).toBe(true);
    expect(index.value).toBe("a");
    expect(index.writable).toBe(false);
    expect(index.enumerable).toBe(true);
    expect(index.configurable).toBe(false);
    expect(length.value).toBe(3);
    expect(length.writable).toBe(false);
    expect(length.enumerable).toBe(false);
    expect(length.configurable).toBe(false);
    expect(foo.value).toBe(10);
    expect(foo.writable).toBe(false);
    expect(foo.configurable).toBe(false);
  });

  test("proxy freeze does not pass a value field in data descriptors", () => {
    const seen = [];
    const target = { value: 1 };
    const proxy = new Proxy(target, {
      defineProperty(_target, key, descriptor) {
        seen.push([key, descriptor.value, descriptor.writable, descriptor.configurable]);
        return Reflect.defineProperty(_target, key, descriptor);
      },
    });

    Object.freeze(proxy);

    expect(seen).toEqual([["value", undefined, false, false]]);
    expect(target.value).toBe(1);
  });
});

describe("Object.freeze on a class", () => {
  test("returns the class and freezes it", () => {
    class Empty {}
    class WithMethod {
      method() {}
    }
    class Base {
      constructor(value) {
        this.value = value;
      }
    }
    class Derived extends Base {}
    const Expression = class {};
    const NamedExpression = class Inner {};

    for (const K of [Empty, WithMethod, Base, Derived, Expression, NamedExpression, class {}]) {
      expect(Object.freeze(K)).toBe(K);
      expect(Object.isFrozen(K)).toBe(true);
      expect(Object.isSealed(K)).toBe(true);
      expect(Object.isExtensible(K)).toBe(false);
    }
  });

  test("makes length, name and prototype non-writable and non-configurable and keeps their values", () => {
    class Point {
      constructor(x, y) {
        this.x = x;
        this.y = y;
      }
    }
    const prototype = Point.prototype;

    expect(Object.getOwnPropertyDescriptors(Point)).toEqual({
      length: { value: 2, writable: false, enumerable: false, configurable: true },
      name: { value: "Point", writable: false, enumerable: false, configurable: true },
      prototype: { value: prototype, writable: false, enumerable: false, configurable: false },
    });

    Object.freeze(Point);

    expect(Object.getOwnPropertyNames(Point)).toEqual(["length", "name", "prototype"]);
    expect(Object.getOwnPropertyDescriptors(Point)).toEqual({
      length: { value: 2, writable: false, enumerable: false, configurable: false },
      name: { value: "Point", writable: false, enumerable: false, configurable: false },
      prototype: { value: prototype, writable: false, enumerable: false, configurable: false },
    });
    expect(Point.length).toBe(2);
    expect(Point.name).toBe("Point");
    expect(Point.prototype).toBe(prototype);
    expect(() => {
      Point.name = "Other";
    }).toThrow(TypeError);
    expect(() => {
      delete Point.length;
    }).toThrow(TypeError);
    expect(() => Object.defineProperty(Point, "name", { value: "Other" })).toThrow(TypeError);
    expect(Point.name).toBe("Point");
  });

  test("freezes static fields and methods", () => {
    const key = Symbol("key");
    class Config {
      static level = 1;
      static [key] = "symbol";
      static describe() {
        return "config";
      }
    }
    Object.freeze(Config);

    expect(() => {
      Config.level = 2;
    }).toThrow(TypeError);
    expect(Config.level).toBe(1);
    expect(() => {
      Config.describe = null;
    }).toThrow(TypeError);
    expect(Config.describe()).toBe("config");
    expect(() => {
      Config.added = 1;
    }).toThrow(TypeError);
    expect(Object.hasOwn(Config, "added")).toBe(false);
    expect(() => {
      delete Config.level;
    }).toThrow(TypeError);
    expect(Object.getOwnPropertyDescriptor(Config, "level")).toEqual({
      value: 1,
      writable: false,
      enumerable: true,
      configurable: false,
    });
    expect(Object.getOwnPropertyDescriptor(Config, key)).toEqual({
      value: "symbol",
      writable: false,
      enumerable: true,
      configurable: false,
    });
    expect(Object.keys(Config)).toEqual(["level"]);
  });

  test("keeps a static accessor callable and makes it non-configurable", () => {
    let stored = 0;
    class Counter {
      static get count() {
        return stored;
      }
      static set count(value) {
        stored = value;
      }
    }
    Object.freeze(Counter);

    Counter.count = 5;
    expect(Counter.count).toBe(5);
    expect(Object.getOwnPropertyDescriptor(Counter, "count").configurable).toBe(false);
    expect(Object.isFrozen(Counter)).toBe(true);
  });

  test("leaves construction, the prototype object and private static state usable", () => {
    class Account {
      static #opened = 0;
      constructor(owner) {
        this.owner = owner;
        Account.#opened = Account.#opened + 1;
      }
      static opened() {
        return Account.#opened;
      }
      greet() {
        return "hi " + this.owner;
      }
    }
    Object.freeze(Account);

    const account = new Account("a");
    expect(account.greet()).toBe("hi a");
    expect(account instanceof Account).toBe(true);
    expect(Account.opened()).toBe(1);
    expect(Object.isFrozen(Account.prototype)).toBe(false);
    Account.prototype.extra = 1;
    expect(new Account("b").extra).toBe(1);
    expect(Account.opened()).toBe(2);
  });

  test("does not freeze a subclass, which still cannot assign an inherited frozen static", () => {
    class Base {
      static shared = 1;
    }
    Object.freeze(Base);
    class Derived extends Base {}
    Derived.own = 2;

    expect(Object.isFrozen(Derived)).toBe(false);
    expect(Derived.own).toBe(2);
    expect(Derived.name).toBe("Derived");
    expect(() => {
      Derived.shared = 3;
    }).toThrow(TypeError);
    expect(Derived.shared).toBe(1);
  });

  test("freezes a class from its own static block", () => {
    class Fixed {
      constructor(a, b, c) {}
      static first = 1;
      static {
        Object.freeze(this);
      }
    }

    expect(Object.isFrozen(Fixed)).toBe(true);
    expect(Fixed.name).toBe("Fixed");
    expect(Fixed.length).toBe(3);
    expect(Fixed.first).toBe(1);
    expect(() => {
      class Late {
        static {
          Object.freeze(this);
        }
        static after = 1;
      }
    }).toThrow(TypeError);
  });

  test("freezes a class whose length or name was deleted, redefined or declared as a static", () => {
    class NoName {}
    delete NoName.name;
    Object.freeze(NoName);
    expect(Object.isFrozen(NoName)).toBe(true);
    expect(Object.getOwnPropertyNames(NoName)).toEqual(["length", "prototype"]);
    expect(NoName.name).toBe("");

    class NoLength {
      constructor(a) {}
    }
    delete NoLength.length;
    Object.freeze(NoLength);
    expect(Object.isFrozen(NoLength)).toBe(true);
    expect(Object.getOwnPropertyNames(NoLength)).toEqual(["name", "prototype"]);
    expect(NoLength.length).toBe(0);

    class Renamed {}
    Object.defineProperty(Renamed, "name", { value: "Other", writable: true, enumerable: true });
    Object.freeze(Renamed);
    expect(Object.getOwnPropertyDescriptor(Renamed, "name")).toEqual({
      value: "Other",
      writable: false,
      enumerable: true,
      configurable: false,
    });

    class Statics {
      static name = "Custom";
      static length = 12;
    }
    Object.freeze(Statics);
    expect(Object.getOwnPropertyDescriptor(Statics, "name")).toEqual({
      value: "Custom",
      writable: false,
      enumerable: true,
      configurable: false,
    });
    expect(Object.getOwnPropertyDescriptor(Statics, "length")).toEqual({
      value: 12,
      writable: false,
      enumerable: true,
      configurable: false,
    });

    class Accessors {
      static get name() {
        return "computed";
      }
    }
    Object.freeze(Accessors);
    expect(Accessors.name).toBe("computed");
    expect(Object.isFrozen(Accessors)).toBe(true);
  });

  test("can be repeated and applied after seal or preventExtensions", () => {
    class Twice {
      static level = 1;
    }
    Object.freeze(Twice);
    expect(Object.freeze(Twice)).toBe(Twice);
    expect(Object.isFrozen(Twice)).toBe(true);

    class Sealed {
      static level = 1;
    }
    Object.seal(Sealed);
    expect(Object.isFrozen(Sealed)).toBe(false);
    Object.freeze(Sealed);
    expect(Object.isFrozen(Sealed)).toBe(true);

    class NonExtensible {
      static level = 1;
    }
    Object.preventExtensions(NonExtensible);
    Object.freeze(NonExtensible);
    expect(Object.isFrozen(NonExtensible)).toBe(true);
    expect(Object.getOwnPropertyDescriptor(NonExtensible, "name").configurable).toBe(false);
  });

  test("reads of length, name and statics agree before and after freezing", () => {
    class Shape {
      constructor(a, b, c) {}
      static sides = 4;
    }
    const read = () => [Shape.length, Shape.name, Shape.sides, typeof Shape.prototype];
    const before = [];
    for (const round of [1, 2, 3, 4]) {
      before.push(read());
    }

    Object.freeze(Shape);

    for (const round of [1, 2, 3, 4]) {
      expect(read()).toEqual([3, "Shape", 4, "object"]);
    }
    expect(before).toEqual([
      [3, "Shape", 4, "object"],
      [3, "Shape", 4, "object"],
      [3, "Shape", 4, "object"],
      [3, "Shape", 4, "object"],
    ]);
  });

  test("freezes a class through a proxy", () => {
    class Target {
      static level = 1;
    }
    const proxy = new Proxy(Target, {});
    Object.freeze(proxy);

    expect(Object.isFrozen(proxy)).toBe(true);
    expect(Object.isFrozen(Target)).toBe(true);
    expect(Object.getOwnPropertyDescriptor(Target, "name")).toEqual({
      value: "Target",
      writable: false,
      enumerable: false,
      configurable: false,
    });
  });

  test("freezes functions of every other kind", () => {
    class Holder {
      method(a) {}
      static staticMethod(a, b) {}
    }
    const object = {
      method(a) {},
      *generator(a) {},
      async asyncMethod(a) {},
    };
    const functions = [
      (a, b) => a + b,
      async (a) => a,
      object.method,
      object.generator,
      object.asyncMethod,
      Holder.prototype.method,
      Holder.staticMethod,
      ((a, b) => a + b).bind(null, 1),
      Holder.bind(null),
    ];

    for (const fn of functions) {
      const name = fn.name;
      const length = fn.length;
      expect(Object.freeze(fn)).toBe(fn);
      expect(Object.isFrozen(fn)).toBe(true);
      expect(Object.getOwnPropertyDescriptor(fn, "name")).toEqual({
        value: name,
        writable: false,
        enumerable: false,
        configurable: false,
      });
      expect(Object.getOwnPropertyDescriptor(fn, "length")).toEqual({
        value: length,
        writable: false,
        enumerable: false,
        configurable: false,
      });
    }
  });

  test("freezes built-in constructors", () => {
    for (const Constructor of [WeakSet, Float32Array, DataView]) {
      const name = Constructor.name;
      const length = Constructor.length;
      expect(Object.freeze(Constructor)).toBe(Constructor);
      expect(Object.isFrozen(Constructor)).toBe(true);
      expect(Constructor.name).toBe(name);
      expect(Constructor.length).toBe(length);
      expect(Object.getOwnPropertyDescriptor(Constructor, "prototype").writable).toBe(false);
    }
    expect(new WeakSet().has({})).toBe(false);
    expect(new Float32Array(2).length).toBe(2);
    expect(new DataView(new ArrayBuffer(4)).byteLength).toBe(4);
  });
});
