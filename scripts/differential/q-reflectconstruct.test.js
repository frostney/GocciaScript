// `Reflect.construct` (and every other entry into the abstract Construct
// operation: a proxy without a construct trap, a bound class, `new` through a
// proxy) has to build the same instance the `new` operator does — instance
// elements included. Field initializers, private fields, and method
// initializers are the half that a second construction path is most likely to
// skip, because they live outside the constructor body.
//
// Bun gates: this is ECMAScript class construction semantics, and the testing
// API is incidental. Vitest is skipped for the same reason as the other
// language suites. See the CLASSIFICATION entry in
// scripts/test-cli-differential.ts.
//
// `new.target` inside a field initializer is left out even though node, bun and
// goccia all agree on it as a script: under `bun test` 1.4.0 the same class
// body reports a defined `new.target`, so gating on it would report bun's
// transpile as a goccia divergence. It is covered against node in
// tests/built-ins/Reflect/construct/instance-elements.js instead.

const outerBinding = 7;

describe("Reflect.construct initializes instance elements", () => {
  test("base class fields", () => {
    class Base {
      a = 1;
    }

    expect(Reflect.construct(Base, []).a).toBe(1);
  });

  test("derived class fields land after the base class fields", () => {
    class Base {
      a = 1;
    }
    class Derived extends Base {
      b = 2;
    }

    const instance = Reflect.construct(Derived, []);
    expect(Object.keys(instance)).toEqual(["a", "b"]);
    expect(instance.a).toBe(1);
    expect(instance.b).toBe(2);
  });

  test("three levels of fields", () => {
    class A {
      a = 1;
    }
    class B extends A {
      b = 2;
    }
    class C extends B {
      c = 3;
    }

    expect(Object.keys(Reflect.construct(C, []))).toEqual(["a", "b", "c"]);
  });

  test("fields run before the constructor body", () => {
    const order = [];
    class Base {
      a = (order.push("field"), 1);

      constructor() {
        order.push("body");
      }
    }

    Reflect.construct(Base, []);
    expect(order).toEqual(["field", "body"]);
  });

  test("a derived constructor sees only the base fields before super() returns", () => {
    const order = [];
    class Base {
      a = (order.push("base-field"), 1);

      constructor() {
        order.push("base-body");
      }
    }
    class Derived extends Base {
      b = (order.push("derived-field"), 2);

      constructor() {
        order.push("before-super");
        super();
        order.push("after-super");
      }
    }

    Reflect.construct(Derived, []);
    expect(order).toEqual([
      "before-super",
      "base-field",
      "base-body",
      "derived-field",
      "after-super",
    ]);
  });

  test("an implicit derived constructor still initializes its own fields", () => {
    class Base {
      constructor() {
        this.seenBeforeOwnFields = this.b;
      }
    }
    class Derived extends Base {
      b = 2;
    }

    const instance = Reflect.construct(Derived, []);
    expect(instance.seenBeforeOwnFields).toBe(undefined);
    expect(instance.b).toBe(2);
  });

  test("arguments reach an inherited constructor", () => {
    class Base {
      constructor(x) {
        this.x = x;
      }
    }
    class Derived extends Base {
      b = 2;
    }

    const instance = Reflect.construct(Derived, [7]);
    expect(instance.x).toBe(7);
    expect(instance.b).toBe(2);
  });

  test("field initializers read the class definition environment", () => {
    class Base {
      v = outerBinding;
    }

    expect(Reflect.construct(Base, []).v).toBe(7);
  });

  test("a class expression closes over its factory's parameter", () => {
    const make = (n) => class {
      v = n;
    };

    expect(Reflect.construct(make(3), []).v).toBe(3);
  });

  test("computed field keys", () => {
    const key = "dyn";
    class Base {
      [key] = 4;
    }

    expect(Reflect.construct(Base, []).dyn).toBe(4);
  });

  test("private fields", () => {
    class Base {
      #secret = 5;

      reveal() {
        return this.#secret;
      }
    }

    expect(Reflect.construct(Base, []).reveal()).toBe(5);
  });

  test("private fields on both halves of a derived class", () => {
    class Base {
      #base = 5;

      revealBase() {
        return this.#base;
      }
    }
    class Derived extends Base {
      #derived = 6;

      revealDerived() {
        return this.#derived;
      }
    }

    const instance = Reflect.construct(Derived, []);
    expect(instance.revealBase()).toBe(5);
    expect(instance.revealDerived()).toBe(6);
  });

  test("accessors see the initialized fields", () => {
    class Base {
      a = 1;

      get double() {
        return this.a * 2;
      }
    }

    expect(Reflect.construct(Base, []).double).toBe(2);
  });
});

describe("Reflect.construct with an explicit newTarget", () => {
  test("newTarget supplies the prototype and the fields still run", () => {
    class Base {
      a = 1;
    }
    class Other {}

    const instance = Reflect.construct(Base, [], Other);
    expect(instance.a).toBe(1);
    expect(Object.getPrototypeOf(instance)).toBe(Other.prototype);
    expect(instance instanceof Other).toBe(true);
    expect(instance instanceof Base).toBe(false);
  });

  test("a derived target keeps both halves of its fields under a foreign newTarget", () => {
    class Base {
      a = 1;
    }
    class Derived extends Base {
      b = 2;
    }
    class Other {}

    const instance = Reflect.construct(Derived, [], Other);
    expect(Object.keys(instance)).toEqual(["a", "b"]);
    expect(Object.getPrototypeOf(instance)).toBe(Other.prototype);
  });

  test("a subclass as newTarget for its own base", () => {
    class Base {
      a = 1;
    }
    class Derived extends Base {
      b = 2;
    }

    const instance = Reflect.construct(Base, [], Derived);
    expect(instance.a).toBe(1);
    expect(instance.b).toBe(undefined);
    expect(Object.getPrototypeOf(instance)).toBe(Derived.prototype);
  });

  test("an ordinary function as newTarget", () => {
    class Base {
      a = 1;
    }
    const marker = { tag: "F" };
    function NewTarget() {}
    NewTarget.prototype = marker;

    const instance = Reflect.construct(Base, [], NewTarget);
    expect(instance.a).toBe(1);
    expect(Object.getPrototypeOf(instance)).toBe(marker);
  });

  test("a newTarget whose prototype is not an object falls back to Object.prototype", () => {
    class Base {
      a = 1;
    }
    function NewTarget() {}
    NewTarget.prototype = 42;

    const instance = Reflect.construct(Base, [], NewTarget);
    expect(instance.a).toBe(1);
    expect(Object.getPrototypeOf(instance)).toBe(Object.prototype);
  });
});

describe("Reflect.construct override returns", () => {
  test("a base constructor returning an object discards the initialized receiver", () => {
    class Base {
      a = 1;

      constructor() {
        return { x: 9 };
      }
    }

    const instance = Reflect.construct(Base, []);
    expect(instance.x).toBe(9);
    expect(instance.a).toBe(undefined);
    expect(instance instanceof Base).toBe(false);
  });

  test("a derived constructor returning an object discards both halves", () => {
    class Base {
      a = 1;
    }
    class Derived extends Base {
      b = 2;

      constructor() {
        super();
        return { x: 9 };
      }
    }

    const instance = Reflect.construct(Derived, []);
    expect(instance.x).toBe(9);
    expect(instance.a).toBe(undefined);
    expect(instance.b).toBe(undefined);
  });

  test("an override return keeps Object.prototype even under a newTarget", () => {
    class Base {
      constructor() {
        return { x: 9 };
      }
    }
    class Other {}

    const instance = Reflect.construct(Base, [], Other);
    expect(Object.getPrototypeOf(instance)).toBe(Object.prototype);
  });

  test("a base constructor's primitive return is ignored", () => {
    class Base {
      a = 1;

      constructor() {
        return 42;
      }
    }

    expect(Reflect.construct(Base, []).a).toBe(1);
  });

  test("a derived constructor's primitive return throws", () => {
    class Base {}
    class Derived extends Base {
      constructor() {
        super();
        return 42;
      }
    }

    expect(() => Reflect.construct(Derived, [])).toThrow(TypeError);
  });
});

describe("other entries into Construct build the same instance", () => {
  test("a proxy without a construct trap forwards to the target", () => {
    class Base {
      a = 1;
    }
    class Derived extends Base {
      b = 2;
    }

    const instance = Reflect.construct(new Proxy(Derived, {}), []);
    expect(Object.keys(instance)).toEqual(["a", "b"]);
  });

  test("a construct trap that forwards through Reflect.construct", () => {
    class Base {
      a = 1;
    }
    const proxy = new Proxy(Base, {
      construct(target, args, newTarget) {
        return Reflect.construct(target, args, newTarget);
      },
    });

    expect(new proxy().a).toBe(1);
  });

  test("a bound class merges bound arguments and keeps the fields", () => {
    class Base {
      a = 1;

      constructor(x) {
        this.x = x;
      }
    }
    const Bound = Base.bind(null, 5);

    const instance = Reflect.construct(Bound, []);
    expect(instance.a).toBe(1);
    expect(instance.x).toBe(5);
    expect(instance instanceof Base).toBe(true);
  });

  test("`new` and Reflect.construct agree shape for shape", () => {
    class Base {
      a = 1;
      #p = 2;

      reveal() {
        return this.#p;
      }
    }
    class Derived extends Base {
      b = 3;
    }

    const built = new Derived();
    const reflected = Reflect.construct(Derived, []);
    expect(Object.keys(reflected)).toEqual(Object.keys(built));
    expect(reflected.a).toBe(built.a);
    expect(reflected.b).toBe(built.b);
    expect(reflected.reveal()).toBe(built.reveal());
    expect(Object.getPrototypeOf(reflected)).toBe(Object.getPrototypeOf(built));
  });
});

describe("construction after Object.setPrototypeOf on the constructor", () => {
  // ES2026 §13.3.7.3 GetSuperConstructor reads the active function object's
  // [[GetPrototypeOf]] when super() runs, so a retarget moves every hop above
  // it — and can point super() at something that is not a constructor at all,
  // which §13.3.7.1 step 3 rejects. Every case here agrees under node, bun and
  // goccia.
  const expectSuperTypeError = (build) => {
    let name = "<no error>";
    try {
      build();
    } catch (error) {
      name = error.name;
    }
    expect(name).toBe("TypeError");
  };

  test("a plain object is not a super constructor", () => {
    class Declared {
      constructor() {
        this.who = "declared";
      }
    }
    class Sub extends Declared {}
    Object.setPrototypeOf(Sub, {});

    expectSuperTypeError(() => new Sub());
    expectSuperTypeError(() => Reflect.construct(Sub, []));
  });

  test("null leaves nothing to resolve super() through", () => {
    class Declared {}
    class Sub extends Declared {}
    Object.setPrototypeOf(Sub, null);

    expectSuperTypeError(() => new Sub());
  });

  test("a retarget crossing the declared chain still terminates", () => {
    // The mutable [[Prototype]] relation and the fixed declared-superclass
    // relation are each acyclic; a resolver that mixed them walked their union
    // and could close on itself.
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
  });

  test("retargeting onto a built-in runs it and drops the declared super", () => {
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
  });

  test("a retarget part-way up moves everything above it", () => {
    class Base {
      b = "b";
    }
    class Middle extends Base {
      m = "m";
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

  test("an explicit leaf constructor's super() follows the retarget too", () => {
    class Base {
      b = "b";
    }
    class Middle extends Base {
      m = "m";
    }
    class Leaf extends Middle {
      l = "l";

      constructor() {
        super();
      }
    }
    class Alt {
      a = "a";
    }
    Object.setPrototypeOf(Middle, Alt);

    expect(Object.keys(new Leaf())).toEqual(["a", "m", "l"]);
  });
});

describe("implicit constructors over a built-in chain", () => {
  test("every class between the subclass and the built-in contributes", () => {
    class Middle extends Array {
      m = "m";
    }
    class Leaf extends Middle {
      l = "l";
    }

    expect(Object.keys(new Leaf())).toEqual(["m", "l"]);
    expect(Object.keys(Reflect.construct(Leaf, []))).toEqual(["m", "l"]);
  });

  test("a borrowed constructor's super() runs the executor once", () => {
    class Tagged extends Promise {
      tag = "t";

      constructor(executor) {
        super(executor);
      }
    }
    class Borrowing extends Tagged {}

    let runs = 0;
    const instance = new Borrowing((resolve) => {
      runs += 1;
      resolve(1);
    });
    expect(runs).toBe(1);
    expect(instance.tag).toBe("t");
  });

  test("a foreign newTarget decides where Map looks its adder up", () => {
    class First extends Map {}
    class Second extends First {}
    class Foreign {}

    let name = "<no error>";
    try {
      Reflect.construct(Second, [[[1, 2]]], Foreign);
    } catch (error) {
      name = error.name;
    }
    expect(name).toBe("TypeError");

    const empty = Reflect.construct(Second, [], Foreign);
    expect(Object.getPrototypeOf(empty)).toBe(Foreign.prototype);
  });
});
