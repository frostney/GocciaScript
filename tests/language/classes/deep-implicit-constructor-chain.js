/*---
description: A long chain of subclasses with default constructors constructs without exhausting the native stack, and keeps the order and receiver of every level
features: [class-inheritance, class-fields-public, extends, super]
---*/

// ES2026 §15.7.14 ClassDefinitionEvaluation step 14.a: a derived class's
// default constructor performs Construct on its [[Prototype]] with the same
// arguments and NewTarget, then InitializeInstanceElements (§7.3.33) on the
// result. Nothing bounds how many such classes a chain may have, so the engine
// walks them without one native frame per class (issue #1665).
const chain = (depth, base, make) => {
  let C = base;
  for (const _ of Array.from({ length: depth })) {
    const B = C;
    C = make(B);
  }
  return C;
};

describe("default constructors forwarding through a short chain", () => {
  test("initialize instance elements from the base outwards", () => {
    const log = [];
    class A { a = log.push("A"); }
    class B extends A { b = log.push("B"); }
    class C extends B {}
    class D extends C { d = log.push("D"); }
    const d = new D();
    expect(log).toEqual(["A", "B", "D"]);
    expect([d.a, d.b, d.d]).toEqual([1, 2, 3]);
  });

  test("pass arguments and new.target to the first explicit constructor", () => {
    let seenTarget;
    class A {
      constructor(x, y) {
        seenTarget = new.target;
        this.sum = x + y;
      }
    }
    const Leaf = chain(5, A, (B) => class extends B {});
    const leaf = new Leaf(2, 3);
    expect(leaf.sum).toBe(5);
    expect(seenTarget).toBe(Leaf);
    expect(Object.getPrototypeOf(leaf)).toBe(Leaf.prototype);
  });

  test("put the fields of the classes above an explicit constructor on the object it returns", () => {
    const replacement = { replaced: true };
    class A { constructor() { return replacement; } }
    class B extends A { b = 1; }
    class C extends B {}
    class D extends C { d = 2; }
    const d = new D();
    expect(d).toBe(replacement);
    expect([d.b, d.d]).toEqual([1, 2]);
  });

  test("leave the outer fields unset when an inner field initializer throws", () => {
    const log = [];
    class A { a = log.push("A"); }
    class B extends A { b = (() => { throw new TypeError("inner"); })(); }
    class C extends B {}
    class D extends C { d = log.push("D"); }
    expect(() => new D()).toThrow(TypeError);
    expect(log).toEqual(["A"]);
  });

  test("follow a [[Prototype]] retargeted part-way up the chain", () => {
    class A { a = 1; }
    class Other { other = 2; }
    class B extends A { b = 3; }
    class C extends B {}
    class D extends C { d = 4; }
    Object.setPrototypeOf(C, Other);
    const d = new D();
    expect([d.a, d.other, d.b, d.d]).toEqual([undefined, 2, undefined, 4]);
  });

  test("throw TypeError when a class in the chain no longer has a constructor parent", () => {
    class A {}
    class B extends A {}
    class C extends B { c = 1; }
    Object.setPrototypeOf(B, null);
    expect(() => new C()).toThrow(TypeError);
  });

  test("reach a built-in at the base", () => {
    const Leaf = chain(10, Array, (B) => class extends B { tag = "leaf"; });
    const leaf = new Leaf(3);
    expect(Array.isArray(leaf)).toBe(true);
    expect(leaf.length).toBe(3);
    expect(leaf.tag).toBe("leaf");
    expect(leaf instanceof Leaf).toBe(true);
  });

  test("run through Reflect.construct with a different newTarget", () => {
    class A { a = 1; }
    const Leaf = chain(5, A, (B) => class extends B { b = 2; });
    class Target {}
    const object = Reflect.construct(Leaf, [], Target);
    expect(Object.getPrototypeOf(object)).toBe(Target.prototype);
    expect(object.a).toBe(1);
    expect(object.b).toBe(2);
  });
});

describe("default constructors forwarding through a 20-class chain", () => {
  // Each class records its position in the chain, 0 for the base.
  const build = (log) => {
    let position = 0;
    return chain(20, class { base = log.push(0); }, (B) => {
      position += 1;
      const own = position;
      return class extends B { [`level${own}`] = log.push(own); };
    });
  };
  const inOrder = Array.from({ length: 21 }, (_, i) => i);

  test("initialize every class in order when constructed with new", () => {
    const log = [];
    const Leaf = build(log);
    const leaf = new Leaf();
    expect(log).toEqual(inOrder);
    expect(leaf.base).toBe(1);
    expect(leaf.level20).toBe(21);
  });

  test("initialize every class in order when reached through super()", () => {
    const log = [];
    const Leaf = build(log);
    class Top extends Leaf {
      constructor() {
        super();
        this.top = log.push("top");
      }
    }
    const top = new Top();
    expect(log).toEqual([...inOrder, "top"]);
    expect(top.top).toBe(22);
  });

  test("initialize every class in order when constructed with Reflect.construct", () => {
    const log = [];
    const Leaf = build(log);
    const leaf = Reflect.construct(Leaf, []);
    expect(log).toEqual(inOrder);
    expect(leaf instanceof Leaf).toBe(true);
  });
});

describe("a 30,000-class chain of default constructors", () => {
  // Built only on a 64-bit host, where the classes fit the memory budget a
  // test worker should spend.
  const is64Bit = typeof Goccia !== "undefined" &&
    ["x86_64", "aarch64", "powerpc64"].includes(Goccia.build.arch);

  test.runIf(is64Bit)("constructs its most derived class", () => {
    const Leaf = chain(30000, class {}, (B) => class extends B {});
    const leaf = new Leaf();
    expect(typeof leaf).toBe("object");
    expect(leaf instanceof Leaf).toBe(true);
    expect(typeof new Leaf()).toBe("object");
  });

  test.runIf(is64Bit)("initializes every class's field from the base outwards", () => {
    let order = 0;
    const Leaf = chain(30000, class { first = order++; }, (B) => class extends B { last = order++; });
    const leaf = new Leaf();
    expect(leaf.first).toBe(0);
    expect(leaf.last).toBe(30000);
  });

  test.runIf(is64Bit)("passes its arguments to an explicit base constructor", () => {
    class Base { constructor(value) { this.value = value; } }
    const Leaf = chain(30000, Base, (B) => class extends B {});
    expect(new Leaf(42).value).toBe(42);
  });
});
