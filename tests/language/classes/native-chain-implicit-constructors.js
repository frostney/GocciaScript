/*---
description: Every class between a subclass and the built-in it extends runs its implicit constructor and initializes its own instance elements
features: [class, class-fields-public, class-fields-private, class-inheritance, Reflect]
---*/

// ES2026 §15.7.14 ClassDefinitionEvaluation step 15a synthesizes
// `constructor(...args) { super(...args); }` for a class that declares none,
// and §13.3.7.1 SuperCall step 11 initializes that class's instance elements
// once super() returns. Construction paths used to collapse the whole chain in
// one walk — find the built-in that has to allocate the receiver, run one
// constructor against it — and every class the walk stepped over silently lost
// both halves. Probed against Node v24.0.1.

const keysOf = (value) => Object.keys(value);

describe("two-level chains reaching a built-in", () => {
  test("an Array chain initializes both classes' fields, base-most first", () => {
    class Middle extends Array {
      mid = "m";
    }
    class Leaf extends Middle {
      leaf = "l";
    }

    const instance = new Leaf();
    expect(instance.mid).toBe("m");
    expect(instance.leaf).toBe("l");
    expect(keysOf(instance)).toEqual(["mid", "leaf"]);
    expect(Array.isArray(instance)).toBe(true);
    expect(instance instanceof Leaf).toBe(true);
  });

  test("a Map chain initializes both classes' fields", () => {
    class Middle extends Map {
      mid = "m";
    }
    class Leaf extends Middle {
      leaf = "l";
    }

    const instance = new Leaf([[1, 2]]);
    expect(instance.mid).toBe("m");
    expect(instance.leaf).toBe("l");
    expect(instance.get(1)).toBe(2);
  });

  test("a Promise chain initializes both classes' fields", () => {
    class Middle extends Promise {
      mid = "m";
    }
    class Leaf extends Middle {
      leaf = "l";
    }

    const instance = new Leaf((resolve) => resolve(1));
    expect(instance.mid).toBe("m");
    expect(instance.leaf).toBe("l");
    return instance.then((value) => {
      expect(value).toBe(1);
    });
  });

  test("an Error chain keeps the subclass prototype", () => {
    class Middle extends Error {
      mid = "m";
    }
    class Leaf extends Middle {
      leaf = "l";
    }

    const instance = new Leaf("boom");
    expect(instance.mid).toBe("m");
    expect(instance.leaf).toBe("l");
    expect(instance.message).toBe("boom");
    expect(Object.getPrototypeOf(instance)).toBe(Leaf.prototype);
    expect(instance instanceof Leaf).toBe(true);
    expect(instance instanceof Middle).toBe(true);
    expect(instance instanceof Error).toBe(true);
  });

  test("Reflect.construct agrees with `new` on the whole chain", () => {
    class Middle extends Array {
      mid = "m";
    }
    class Leaf extends Middle {
      leaf = "l";
    }

    expect(keysOf(Reflect.construct(Leaf, []))).toEqual(["mid", "leaf"]);
  });
});

describe("three-level chains reaching a built-in", () => {
  test("every implicit constructor in the chain contributes", () => {
    class First extends Array {
      first = 1;
    }
    class Second extends First {
      second = 2;
    }
    class Third extends Second {
      third = 3;
    }

    expect(keysOf(new Third())).toEqual(["first", "second", "third"]);
    expect(keysOf(Reflect.construct(Third, []))).toEqual([
      "first",
      "second",
      "third",
    ]);
  });

  test("an explicit constructor part-way up still runs", () => {
    const order = [];

    class First extends Array {
      first = 1;

      constructor() {
        super();
        order.push("First");
      }
    }
    class Second extends First {
      second = 2;
    }
    class Third extends Second {
      third = 3;
    }

    const instance = new Third();
    expect(order).toEqual(["First"]);
    expect(keysOf(instance)).toEqual(["first", "second", "third"]);
  });
});

describe("private instance elements on an intermediate class", () => {
  test("the brand is stamped once and the initializer runs", () => {
    class Middle extends Array {
      #mid = "m";

      readMid() {
        return this.#mid;
      }
    }
    class Leaf extends Middle {
      #leaf = "l";

      readLeaf() {
        return this.#leaf;
      }
    }

    const instance = new Leaf();
    expect(instance.readMid()).toBe("m");
    expect(instance.readLeaf()).toBe("l");

    const constructed = Reflect.construct(Leaf, []);
    expect(constructed.readMid()).toBe("m");
    expect(constructed.readLeaf()).toBe("l");
  });
});

describe("a chain with no built-in is unaffected", () => {
  test("plain classes keep initializing in declaration order", () => {
    class First {
      first = 1;
    }
    class Second extends First {
      second = 2;
    }
    class Third extends Second {
      third = 3;
    }

    expect(keysOf(new Third())).toEqual(["first", "second", "third"]);
  });
});
