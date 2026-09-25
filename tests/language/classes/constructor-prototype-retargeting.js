/*---
description: Object.setPrototypeOf on a constructor moves super() for a derived class and is inert for a base one
features: [class, class-inheritance, Reflect]
---*/

// ES2026 §15.7.14 ClassDefinitionEvaluation step 9 fixes [[ConstructorKind]]
// when the class is evaluated, and it never changes afterwards. What the
// constructor object's own [[Prototype]] decides is where super() goes:
// §13.3.7.3 GetSuperConstructor reads it when the call runs, so a ~derived~
// class follows an Object.setPrototypeOf — including onto a built-in, or onto
// something that is not a constructor at all, which §13.3.7.1 SuperCall step 3
// rejects. A ~base~ class has no super() to resolve, so the same mutation only
// affects ordinary property lookup on the constructor: static-method
// inheritance and static `super`.
//
// Every expectation here was probed against Node v24.0.1.

describe("retargeting an ordinary class", () => {
  test("the retargeted class does not run the new prototype's constructor", () => {
    class PlainBase {
      constructor() {
        this.tag = "pb";
      }
    }
    class Retarget {}
    Object.setPrototypeOf(Retarget, PlainBase);

    expect(new Retarget().tag).toBe(undefined);
    expect(Reflect.construct(Retarget, []).tag).toBe(undefined);
    expect(Object.keys(new Retarget())).toEqual([]);
  });

  test("the retargeted class does not inherit instance fields either", () => {
    class Fielded {
      own = 1;
    }
    class Retarget {}
    Object.setPrototypeOf(Retarget, Fielded);

    expect(Object.keys(new Retarget())).toEqual([]);
  });

  test("the instance still gets the retargeted class's own prototype", () => {
    class PlainBase {}
    class Retarget {}
    Object.setPrototypeOf(Retarget, PlainBase);

    const instance = new Retarget();
    expect(Object.getPrototypeOf(instance)).toBe(Retarget.prototype);
    expect(instance instanceof Retarget).toBe(true);
    expect(instance instanceof PlainBase).toBe(false);
  });

  test("static methods do follow the mutated chain", () => {
    class StaticSource {
      static make() {
        return "made";
      }
    }
    class Retarget {}
    Object.setPrototypeOf(Retarget, StaticSource);

    expect(Retarget.make()).toBe("made");
  });
});

describe("retargeting onto a class whose chain reaches a built-in", () => {
  test("no built-in receiver is allocated", () => {
    class WithMap extends Map {}
    class Retarget {}
    Object.setPrototypeOf(Retarget, WithMap);

    const constructed = Reflect.construct(Retarget, []);
    expect(constructed instanceof Map).toBe(false);
    expect(Object.getPrototypeOf(constructed)).toBe(Retarget.prototype);

    const created = new Retarget();
    expect(created instanceof Map).toBe(false);
    expect(Object.getPrototypeOf(created)).toBe(Retarget.prototype);
  });

  test("an Array-backed retarget is an ordinary object too", () => {
    class WithArray extends Array {}
    class Retarget {}
    Object.setPrototypeOf(Retarget, WithArray);

    expect(Array.isArray(new Retarget())).toBe(false);
    expect(Array.isArray(Reflect.construct(Retarget, []))).toBe(false);
  });
});

describe("retargeting onto something that is not a constructor", () => {
  // §13.3.7.3 GetSuperConstructor hands super() whatever the constructor's
  // [[Prototype]] is, and §13.3.7.1 SuperCall step 3 rejects it when that is
  // not a constructor. Probed against Node v24.0.1: every one of these throws
  // a TypeError.
  const expectSuperTypeError = (build) => {
    let name = "<no error>";
    try {
      build();
    } catch (error) {
      name = error.name;
    }
    expect(name).toBe("TypeError");
  };

  const makeSubclass = () => {
    class Declared {
      constructor() {
        this.who = "declared";
      }
    }
    return class Sub extends Declared {};
  };

  test("a plain object is not a super constructor", () => {
    const Sub = makeSubclass();
    Object.setPrototypeOf(Sub, {});

    expectSuperTypeError(() => new Sub());
    expectSuperTypeError(() => Reflect.construct(Sub, []));
    expectSuperTypeError(() => new (Sub.bind(null))());
  });

  test("null leaves nothing to resolve super() through", () => {
    const Sub = makeSubclass();
    Object.setPrototypeOf(Sub, null);

    expectSuperTypeError(() => new Sub());
    expectSuperTypeError(() => Reflect.construct(Sub, []));
  });

  test("an arrow function is callable but not constructable", () => {
    const Sub = makeSubclass();
    Object.setPrototypeOf(Sub, (x) => x);

    expectSuperTypeError(() => new Sub());
  });

  test("Function.prototype is not a constructor either", () => {
    const Sub = makeSubclass();
    Object.setPrototypeOf(Sub, Object.getPrototypeOf(Object));

    expectSuperTypeError(() => new Sub());
  });

  test("an explicit constructor's super() throws the same way", () => {
    class Declared {
      constructor() {
        this.who = "declared";
      }
    }
    class Sub extends Declared {
      constructor() {
        super();
      }
    }
    Object.setPrototypeOf(Sub, {});

    expectSuperTypeError(() => new Sub());
    expectSuperTypeError(() => Reflect.construct(Sub, []));
  });

  test("mixing a retarget with a declared chain terminates", () => {
    // The two relations are individually acyclic — ordinary [[SetPrototypeOf]]
    // rejects a cycle, and a declared superclass is fixed at definition time —
    // but a resolver that followed the mutable one and fell back to the
    // declared one walked a union of the two that could close on itself.
    class Base {
      b = 1;
    }
    class Middle extends Base {
      m = 2;
    }
    class Leaf extends Middle {
      l = 3;
    }
    Object.setPrototypeOf(Leaf, {});
    Object.setPrototypeOf(Middle, Leaf);

    expectSuperTypeError(() => new Leaf());
    expectSuperTypeError(() => Reflect.construct(Leaf, []));
  });
});

describe("retargeting onto a built-in constructor", () => {
  // A built-in exposed as a function value rather than as a class value is a
  // perfectly good super constructor, and the declared one stops running.
  // Probed against Node v24.0.1.
  test("Error runs and the declared superclass does not", () => {
    class Declared {
      constructor() {
        this.who = "declared";
      }
    }
    class Sub extends Declared {}
    Object.setPrototypeOf(Sub, Error);

    const instance = new Sub("boom");
    expect(instance.message).toBe("boom");
    expect(instance.who).toBe(undefined);
    expect(Object.getPrototypeOf(instance)).toBe(Sub.prototype);
  });

  test("a bound class is constructable and runs", () => {
    class Declared {
      constructor() {
        this.who = "declared";
      }
    }
    class Alt {
      constructor() {
        this.who = "alt";
      }
    }
    class Sub extends Declared {}
    Object.setPrototypeOf(Sub, Alt.bind(null));

    expect(new Sub().who).toBe("alt");
  });
});

describe("an explicit constructor over a retargeted class", () => {
  // §13.3.7.3 is read when super() runs, so an explicit body sees the same
  // target an implicit one would. Probed against Node v24.0.1.
  test("super() runs the retargeted constructor", () => {
    class Declared {
      constructor() {
        this.who = "declared";
      }
    }
    class Alt {
      constructor() {
        this.who = "alt";
      }
    }
    class Sub extends Declared {
      constructor() {
        super();
        this.tail = 1;
      }
    }
    Object.setPrototypeOf(Sub, Alt);

    expect(new Sub().who).toBe("alt");
    expect(new Sub().tail).toBe(1);
    expect(Reflect.construct(Sub, []).who).toBe("alt");
  });

  test("super() through a retargeted built-in allocates its receiver", () => {
    class Declared {}
    class Sub extends Declared {
      constructor() {
        super();
        this.tail = 1;
      }
    }
    Object.setPrototypeOf(Sub, Array);

    const instance = new Sub();
    expect(Array.isArray(instance)).toBe(true);
    expect(instance.tail).toBe(1);
  });
});

describe("retargeting a class in the middle of a chain", () => {
  // Each hop is resolved when super() runs, so a retarget part-way up moves
  // everything above it. Probed against Node v24.0.1.
  const build = (LeafShape) => {
    class Base {
      b = "b";
    }
    class Middle extends Base {
      m = "m";
    }
    class Alt {
      a = "a";
    }
    const Leaf = LeafShape(Middle);
    Object.setPrototypeOf(Middle, Alt);
    return Leaf;
  };

  test("an implicit leaf follows the retargeted middle", () => {
    const Leaf = build(
      (Middle) =>
        class Leaf extends Middle {
          l = "l";
        },
    );
    expect(Object.keys(new Leaf())).toEqual(["a", "m", "l"]);
    expect(Object.keys(Reflect.construct(Leaf, []))).toEqual(["a", "m", "l"]);
  });

  test("an explicit leaf's super() follows it too", () => {
    const Leaf = build(
      (Middle) =>
        class Leaf extends Middle {
          l = "l";

          constructor() {
            super();
          }
        },
    );
    expect(Object.keys(new Leaf())).toEqual(["a", "m", "l"]);
  });

  test("a middle with its own constructor still forwards to the retarget", () => {
    class Base {
      b = "b";
    }
    class Middle extends Base {
      m = "m";

      constructor() {
        super();
      }
    }
    class Leaf extends Middle {
      l = "l";
    }
    class Alt {
      a = "a";
    }
    Object.setPrototypeOf(Middle, Alt);

    expect(Object.keys(new Leaf())).toEqual(["a", "m", "l"]);
  });

  test("a four-deep chain drops everything above the retarget", () => {
    class A0 {
      a0 = 0;
    }
    class A1 extends A0 {
      a1 = 1;
    }
    class A2 extends A1 {
      a2 = 2;
    }
    class A3 extends A2 {
      a3 = 3;
    }
    class Alt {
      alt = "alt";
    }
    Object.setPrototypeOf(A2, Alt);

    expect(Object.keys(new A3())).toEqual(["alt", "a2", "a3"]);
  });
});

describe("retargeting a derived class", () => {
  // §13.3.7.3 GetSuperConstructor reads the active function object's
  // [[GetPrototypeOf]], so a ~derived~ constructor's super() — explicit or the
  // implicit one of §15.7.14 step 15a — does follow the mutated chain. Only
  // the ~base~ case above is unaffected, because it has no super() to resolve.
  test("super() runs the new prototype's constructor, not the declared one", () => {
    class Declared {
      constructor() {
        this.from = "declared";
      }
    }
    class Other {
      constructor() {
        this.from = "other";
      }
    }
    class Sub extends Declared {}
    Object.setPrototypeOf(Sub, Other);

    expect(new Sub().from).toBe("other");
    expect(Reflect.construct(Sub, []).from).toBe("other");
  });

  test("the new prototype's instance fields run before the subclass's", () => {
    class Declared {}
    class Fielded {
      own = 1;
    }
    class Sub extends Declared {
      mine = 2;
    }
    Object.setPrototypeOf(Sub, Fielded);

    expect(Object.keys(new Sub())).toEqual(["own", "mine"]);
    expect(Object.keys(Reflect.construct(Sub, []))).toEqual(["own", "mine"]);
  });
});
