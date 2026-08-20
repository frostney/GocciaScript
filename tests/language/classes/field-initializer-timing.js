/*---
description: Instance field initializers run in the class's definition environment, at the point [[Construct]] reaches them
features: [classes, private-fields]
---*/

// ES2026 §10.2.2 [[Construct]] step 5b initializes a *base* class's instance
// elements after `this` is bound and before the constructor body runs, while
// §13.3.7.1 SuperCall step 11 initializes a *derived* class's elements when its
// own super() returns. §15.7.10 ClassFieldDefinitionEvaluation step 2b makes
// every initializer a closure over the class's definition environment, not over
// whichever scope happens to call `new`.

describe("base and derived field initializer timing", () => {
  test("a base class initializes its fields before its constructor body", () => {
    class Base {
      f = 1;

      constructor() {
        this.afterF = this.f + 1;
      }
    }

    const base = new Base();
    expect(Object.keys(base)).toEqual(["f", "afterF"]);
    expect(base.afterF).toBe(2);
  });

  test("a derived class initializes its fields when super() returns", () => {
    class Base {
      constructor() {
        this.x = 1;
      }
    }

    class Derived extends Base {
      f = 2;

      constructor() {
        super();
        this.y = 3;
      }
    }

    expect(Object.keys(new Derived())).toEqual(["x", "f", "y"]);
  });

  test("a derived field initializer sees what the base constructor assigned", () => {
    class Base {
      constructor() {
        this.x = 10;
      }
    }

    class Derived extends Base {
      copied = this.x;
    }

    expect(new Derived().copied).toBe(10);
  });

  test("a base field initializer cannot see its own constructor's writes", () => {
    class Base {
      copied = this.x;

      constructor() {
        this.x = 11;
      }
    }

    expect(new Base().copied).toBeUndefined();
  });

  test("each layer of a three-class chain contributes keys in construction order", () => {
    class A {
      constructor() {
        this.x = 1;
      }
    }

    class B extends A {
      f = 2;

      constructor() {
        super();
        this.y = 3;
      }
    }

    class C extends B {
      g = 4;

      constructor() {
        super();
        this.z = 5;
      }
    }

    expect(Object.keys(new C())).toEqual(["x", "f", "y", "g", "z"]);
  });

  test("implicit constructors keep the same ordering across the chain", () => {
    class A {
      constructor() {
        this.x = 1;
      }
    }

    class B extends A {
      f = 2;
    }

    class C extends B {
      g = 3;
    }

    expect(Object.keys(new B())).toEqual(["x", "f"]);
    expect(Object.keys(new C())).toEqual(["x", "f", "g"]);
  });

  test("an intermediate class without a constructor still runs its ancestor's", () => {
    class A {
      constructor() {
        this.x = 1;
      }
    }

    class B extends A {}

    class C extends B {
      g = 4;

      constructor() {
        super();
        this.z = 5;
      }
    }

    expect(Object.keys(new C())).toEqual(["x", "g", "z"]);
  });

  test("initializer side effects are observable between super() and the body", () => {
    const order = [];

    class Base {
      constructor() {
        order.push("base body");
      }
    }

    class Derived extends Base {
      f = order.push("derived field");

      constructor() {
        order.push("before super");
        super();
        order.push("after super");
      }
    }

    new Derived();
    expect(order).toEqual([
      "before super",
      "base body",
      "derived field",
      "after super",
    ]);
  });

  test("side effects interleave per layer down a derived-of-derived chain", () => {
    const order = [];

    class A {
      f = order.push("A field");

      constructor() {
        order.push("A body");
      }
    }

    class B extends A {
      f2 = order.push("B field");

      constructor() {
        order.push("B before super");
        super();
        order.push("B after super");
      }
    }

    class C extends B {
      f3 = order.push("C field");

      constructor() {
        order.push("C before super");
        super();
        order.push("C after super");
      }
    }

    new C();
    expect(order).toEqual([
      "C before super",
      "B before super",
      "A field",
      "A body",
      "B field",
      "B after super",
      "C field",
      "C after super",
    ]);
  });

  test("private fields follow the same per-layer timing", () => {
    class Base {
      #p = "base-private";

      constructor() {
        this.x = 1;
      }

      basePrivate() {
        return this.#p;
      }
    }

    class Derived extends Base {
      #q = "derived-private";

      constructor() {
        super();
        this.y = 2;
      }

      derivedPrivate() {
        return this.#q;
      }
    }

    const derived = new Derived();
    expect(Object.keys(derived)).toEqual(["x", "y"]);
    expect(derived.basePrivate()).toBe("base-private");
    expect(derived.derivedPrivate()).toBe("derived-private");
  });
});

describe("field initializers close over the class definition environment", () => {
  const makeLabelled = () => {
    const PREFIX = "id-";

    class Labelled {
      label = PREFIX + "labelled";

      constructor(n) {
        this.n = n;
      }
    }

    return Labelled;
  };

  const makeStamped = () => {
    const PREFIX = "far-";

    class Base {
      base = PREFIX + "base";

      constructor() {
        this.mid = PREFIX + "mid";
      }
    }

    class Stamped extends Base {
      stamp = PREFIX + "stamp";

      #secret = PREFIX + "secret";

      constructor() {
        super();
        this.tail = PREFIX + "tail";
      }

      secret() {
        return this.#secret;
      }
    }

    return Stamped;
  };

  test("constructing from a scope without the binding still resolves it", () => {
    const Labelled = makeLabelled();
    const labelled = new Labelled(7);

    expect(Object.keys(labelled)).toEqual(["label", "n"]);
    expect(labelled.label).toBe("id-labelled");
    expect(labelled.n).toBe(7);
  });

  test("a derived class resolves the binding at the far call site too", () => {
    const Stamped = makeStamped();
    const stamped = new Stamped();

    expect(Object.keys(stamped)).toEqual(["base", "mid", "stamp", "tail"]);
    expect(stamped.base).toBe("far-base");
    expect(stamped.stamp).toBe("far-stamp");
    expect(stamped.secret()).toBe("far-secret");
  });

  test("a shadowing binding at the call site does not capture the initializer", () => {
    const makeTagged = () => {
      const TAG = "definition";

      class Tagged {
        tag = TAG;
      }

      return Tagged;
    };

    const Tagged = makeTagged();
    const TAG = "call-site";

    expect(new Tagged().tag).toBe("definition");
    expect(TAG).toBe("call-site");
  });

  test("a block-scoped class keeps its block bindings after the block exits", () => {
    let Boxed;

    {
      const INNER = "boxed";

      class Inner {
        tag = INNER;
      }

      Boxed = Inner;
    }

    expect(new Boxed().tag).toBe("boxed");
  });

  test("a field initializer resolves the class's own inner name binding", () => {
    const Named = class Inner {
      self = Inner;
    };

    expect(new Named().self).toBe(Named);
  });
});
