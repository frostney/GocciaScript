/*---
description: A derived constructor that returns without calling super() raises a ReferenceError through every construction route
features: [class-inheritance, Reflect, class]
---*/

// ES2026 §10.2.2 [[Construct]] step 13.c. The check belongs to the constructor
// that returned, so it has to fire wherever that constructor was entered from:
// `new`, a subclass's super(), Reflect.construct, and a bound wrapper all
// reach it through different machinery. In bytecode mode the compiler emits an
// unconditional implicit `undefined` return and the derived-this guard only
// covers `this` *access*, so a body that touches neither has no error of its
// own to raise. Probed against Node v24.0.1: every one of these throws.
//
// Known deviation, identical in both modes and therefore not a parity break: a
// subclass with no constructor of its own (`class L extends Middle {}`) still
// constructs successfully. Its implicit constructor forwards through a
// different set of paths, none of which consult the flag yet.

class Base {}

class Middle extends Base {
  constructor() {
    // Deliberately never calls super() and never reads `this`.
  }
}

class Leaf extends Middle {
  constructor() {
    super();
  }
}

const expectMissingSuper = (build) => {
  let name = "<no error>";
  try {
    build();
  } catch (error) {
    name = error.name;
  }
  expect(name).toBe("ReferenceError");
};

describe("a derived constructor that never calls super()", () => {
  test("`new` on the constructor itself throws", () => {
    expectMissingSuper(() => new Middle());
  });

  test("a subclass's explicit super() throws", () => {
    expectMissingSuper(() => new Leaf());
  });

  test("Reflect.construct throws", () => {
    expectMissingSuper(() => Reflect.construct(Middle, []));
  });

  test("construction through a bound wrapper throws", () => {
    expectMissingSuper(() => new (Middle.bind(null))());
  });

  test("a constructor that does call super() is unaffected", () => {
    class Ok extends Base {
      seq = 1;

      constructor() {
        super();
        this.tail = 2;
      }
    }

    expect(Object.keys(new Ok())).toEqual(["seq", "tail"]);
    expect(Object.keys(Reflect.construct(Ok, []))).toEqual(["seq", "tail"]);
    expect(Object.keys(new (Ok.bind(null))())).toEqual(["seq", "tail"]);
  });

  test("an explicit object return stands in for super()", () => {
    // §10.2.2 step 13.a: returning an Object is the other way a derived
    // constructor can finish, and it is checked before step 13.c.
    class Returns extends Base {
      constructor() {
        return { replaced: true };
      }
    }

    expect(new Returns().replaced).toBe(true);
    expect(Reflect.construct(Returns, []).replaced).toBe(true);
  });
});
