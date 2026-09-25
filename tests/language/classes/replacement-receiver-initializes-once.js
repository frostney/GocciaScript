/*---
description: A derived class whose super() returns a replacement object initializes its instance elements exactly once on it
features: [class-inheritance, class-fields, private-fields, Reflect, class]
---*/

// ES2026 §10.2.2 step 12 and §13.3.7.1 step 11: when the super constructor
// returns an object, that object becomes the receiver and the derived class's
// instance elements run on it — once. Every construction route reaches the
// derived constructor through different machinery, and a route that both
// initializes the replacement receiver and then replays the initializers
// afterwards evaluates every field twice and stamps the private brand twice,
// which the second stamp reports as a repeated super() call.

let ticks = 0;

class ReplacingBase {
  constructor() {
    return { replaced: true };
  }
}

class Derived extends ReplacingBase {
  seq = ++ticks;

  #brand = "derived";

  constructor() {
    super();
    this.tail = "tail";
  }

  // The replacement object keeps Object.prototype, so the brand is only
  // reachable through a static of the class that stamped it.
  static readBrand(instance) {
    return instance.#brand;
  }
}

const expectInitializedOnce = (build) => {
  const before = ticks;
  const instance = build();

  expect(ticks - before).toBe(1);
  expect(instance.replaced).toBe(true);
  expect(instance.seq).toBe(ticks);
  expect(instance.tail).toBe("tail");
  expect(Derived.readBrand(instance)).toBe("derived");
  expect(Object.getPrototypeOf(instance)).toBe(Object.prototype);
  expect(Object.keys(instance)).toEqual(["replaced", "seq", "tail"]);
};

describe("a super() that returns a replacement object", () => {
  test("`new` initializes the replacement once", () => {
    expectInitializedOnce(() => new Derived());
  });

  test("Reflect.construct initializes the replacement once", () => {
    expectInitializedOnce(() => Reflect.construct(Derived, []));
  });

  test("a bound wrapper initializes the replacement once", () => {
    expectInitializedOnce(() => new (Derived.bind(null))());
  });

  test("a further subclass's super() initializes each layer once", () => {
    class Leaf extends Derived {
      leafSeq = ++ticks;

      #leafBrand = "leaf";

      constructor() {
        super();
        this.leafTail = "leaf-tail";
      }

      static readLeafBrand(instance) {
        return instance.#leafBrand;
      }
    }

    const before = ticks;
    const leaf = new Leaf();

    expect(ticks - before).toBe(2);
    expect(leaf.replaced).toBe(true);
    expect(Derived.readBrand(leaf)).toBe("derived");
    expect(Leaf.readLeafBrand(leaf)).toBe("leaf");
    expect(Object.keys(leaf)).toEqual([
      "replaced",
      "seq",
      "tail",
      "leafSeq",
      "leafTail",
    ]);
  });
});
