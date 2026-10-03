/*---
description: A public auto-accessor keeps its value in a private storage slot, not in an own property
features: [decorators, auto-accessor]
---*/

// proposal-decorators ClassFieldDefinitionEvaluation (tc39/ecma262#2417):
// `accessor x` stores its value under a new Private Name, and its getter and
// setter use PrivateGet and PrivateSet, which throw TypeError on an object
// without that private name.

const descriptorOf = (target, key) => Object.getOwnPropertyDescriptor(target, key);

describe("public auto-accessor storage", () => {
  test("is not an own property of the instance", () => {
    class C {
      accessor x = 1;
    }

    const c = new C();

    expect(c.x).toBe(1);
    expect(Object.keys(c)).toEqual([]);
    expect(Reflect.ownKeys(c)).toEqual([]);
    expect(JSON.stringify(c)).toBe("{}");
    expect(Object.hasOwn(c, "__accessor_x")).toBe(false);
  });

  test("is not reachable through a property name", () => {
    class C {
      accessor x = 1;
    }

    const c = new C();
    c.__accessor_x = 9;

    expect(c.x).toBe(1);
    delete c.__accessor_x;
    expect(c.x).toBe(1);
    c.x = 2;
    expect(c.x).toBe(2);
    expect(Object.keys(c)).toEqual([]);
  });

  test("does not collide with a field of any name", () => {
    class C {
      __accessor_x = "field";
      accessor x = 1;
    }

    const c = new C();

    expect(c.x).toBe(1);
    expect(c.__accessor_x).toBe("field");
    c.x = 2;
    expect(c.__accessor_x).toBe("field");
  });

  test("getter and setter throw TypeError on an object without the storage", () => {
    class C {
      accessor x = 1;
    }
    const { get, set } = descriptorOf(C.prototype, "x");
    const plain = {};

    expect(() => get.call(plain)).toThrow(TypeError);
    expect(() => set.call(plain, 3)).toThrow(TypeError);
    expect(Object.keys(plain)).toEqual([]);
    expect(() => get.call(C.prototype)).toThrow(TypeError);
  });

  test("each class has its own storage, even for the same source", () => {
    const make = () => class {
      accessor v = 1;
    };
    const First = make();
    const Second = make();

    expect(() => descriptorOf(First.prototype, "v").get.call(new Second())).toThrow(TypeError);
  });

  test("a subclass accessor of the same name has separate storage", () => {
    class Base {
      accessor x = "base";
    }
    class Derived extends Base {
      accessor x = "derived";
    }

    const d = new Derived();

    expect(d.x).toBe("derived");
    expect(descriptorOf(Base.prototype, "x").get.call(d)).toBe("base");
    expect(() => descriptorOf(Derived.prototype, "x").get.call(new Base())).toThrow(TypeError);
  });

  test("a redeclared accessor gets its own storage and the last one wins", () => {
    const symbol = Symbol("key");
    class C {
      accessor x = 0;
      accessor x = 1;
      accessor [symbol] = 2;
      accessor [symbol] = 3;
    }

    const c = new C();

    expect(c.x).toBe(1);
    expect(c[symbol]).toBe(3);
  });

  test("computed, string and symbol keys all use private storage", () => {
    const name = "computed";
    const symbol = Symbol("symbol");
    class C {
      accessor [name] = 1;
      accessor [symbol] = 2;
      accessor "a:b" = 3;
      accessor $value = 4;
      accessor 5 = 5;
    }

    const c = new C();

    expect([c.computed, c[symbol], c["a:b"], c.$value, c[5]]).toEqual([1, 2, 3, 4, 5]);
    expect(Reflect.ownKeys(c)).toEqual([]);
  });

  test("is added to an object returned by a base constructor", () => {
    class Base {
      constructor(target) {
        return target;
      }
    }
    class Stamp extends Base {
      accessor tag = "stamped";
    }
    const target = {};

    new Stamp(target);

    expect(descriptorOf(Stamp.prototype, "tag").get.call(target)).toBe("stamped");
    expect(Object.keys(target)).toEqual([]);
    expect(() => new Stamp(target)).toThrow(TypeError);
  });

  test("cannot be added to a non-extensible object", () => {
    class Base {
      constructor() {
        Object.preventExtensions(this);
      }
    }
    class Derived extends Base {
      accessor x = 1;
    }

    expect(() => new Derived()).toThrow(TypeError);
  });

  test("stays writable on a frozen instance", () => {
    class C {
      accessor x = 1;
    }
    const c = Object.freeze(new C());

    c.x = 2;

    expect(c.x).toBe(2);
  });
});

describe("public auto-accessor property", () => {
  test("is an enumerable, configurable accessor with built-in getter and setter", () => {
    class C {
      accessor x = 1;
    }

    const descriptor = descriptorOf(C.prototype, "x");

    expect(descriptor.enumerable).toBe(true);
    expect(descriptor.configurable).toBe(true);
    expect(descriptor.get.name).toBe("get x");
    expect(descriptor.get.length).toBe(0);
    expect(descriptor.set.name).toBe("set x");
    expect(descriptor.set.length).toBe(1);
    expect(Object.keys(C.prototype)).toEqual(["x"]);
  });

  test("names a symbol-keyed getter and setter after the symbol", () => {
    const symbol = Symbol("key");
    class C {
      accessor [symbol] = 1;
    }

    const descriptor = descriptorOf(C.prototype, symbol);

    expect(descriptor.get.name).toBe("get [key]");
    expect(descriptor.set.name).toBe("set [key]");
  });

  test("is defined in element order with getters and setters of the same name", () => {
    class AccessorFirst {
      #x = "private";
      accessor x = "auto";
      get x() {
        return this.#x;
      }
      set x(value) {
        this.#x = value;
      }
    }
    class GetterFirst {
      #x = "private";
      get x() {
        return this.#x;
      }
      accessor x = "auto";
      set x(value) {
        this.#x = value;
      }
      peek() {
        return this.#x;
      }
    }
    class AccessorLast {
      get x() {
        return "getter";
      }
      set x(value) {}
      accessor x = "auto";
    }

    expect(new AccessorFirst().x).toBe("private");

    const getterFirst = new GetterFirst();
    getterFirst.x = "set";
    expect(getterFirst.x).toBe("auto");
    expect(getterFirst.peek()).toBe("set");

    const accessorLast = new AccessorLast();
    accessorLast.x = "set";
    expect(accessorLast.x).toBe("set");
  });
});

describe("auto-accessor initializer", () => {
  // CreateFieldInitializerFunction sets [[ClassFieldInitializerName]] to the
  // accessor's name, so an anonymous function is named exactly as it would be
  // in a field of the same name, never after the storage.
  test("names an anonymous function like a field of the same name", () => {
    const key = "computed";
    class WithAccessors {
      accessor plain = () => {};
      accessor [key] = () => {};
      accessor #hidden = () => {};
      hiddenName() {
        return this.#hidden.name;
      }
    }
    class WithFields {
      plain = () => {};
      [key] = () => {};
      #hidden = () => {};
      hiddenName() {
        return this.#hidden.name;
      }
    }

    const accessors = new WithAccessors();
    const fields = new WithFields();

    expect(accessors.plain.name).toBe(fields.plain.name);
    expect(accessors.computed.name).toBe(fields.computed.name);
    expect(accessors.hiddenName()).toBe(fields.hiddenName());
    expect(accessors.plain.name).not.toContain("storage");
  });
});
