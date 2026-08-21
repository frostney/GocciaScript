/*---
description: Object.setPrototypeOf on a constructor does not change which superclass its [[Construct]] runs
features: [class, class-inheritance, Reflect]
---*/

// ES2026 §15.7.14 ClassDefinitionEvaluation step 9 fixes [[ConstructorKind]]
// and the constructor's [[HomeObject]]/super binding when the class is
// evaluated. §10.2.2 [[Construct]] then runs the body that class was defined
// with; the constructor object's own [[Prototype]] only takes part in ordinary
// property lookup — static-method inheritance and static `super` — never in
// choosing whose constructor body executes or which receiver is allocated.
//
// Probed against Node v24.0.1: an ordinary class retargeted onto another class
// constructs a plain object with its own prototype and runs neither the other
// class's constructor nor its allocator.

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
