/*---
description: An anonymous class expression with a heritage, evaluated in a nested block or loop, leaves every binding of its function intact for closures that outlive the function
features: [classes]
---*/

describe("anonymous class with extends in a nested block", () => {
  test("a binding reassigned to class extends itself is still the class after the function returns", () => {
    const make = () => {
      let C = class {};
      { C = class extends C {}; }
      return () => typeof new C();
    };
    expect(make()()).toBe("object");
  });

  test("the escaping closure sees the class that was assigned", () => {
    const make = () => {
      let C = class {};
      const first = C;
      { C = class extends C {}; }
      const inner = C;
      return () => [C === inner, C === first, Object.getPrototypeOf(C) === first];
    };
    expect(make()()).toEqual([true, false, true]);
  });

  test("the assigned class keeps its inferred name", () => {
    const make = () => {
      let C = class {};
      { C = class extends C {}; }
      return () => [C.name, Object.getPrototypeOf(C).name];
    };
    expect(make()()).toEqual(["C", "C"]);
  });

  test("Reflect.construct accepts the class", () => {
    const make = () => {
      let C = class {};
      { C = class extends C {}; }
      return () => Reflect.construct(C, []) instanceof C;
    };
    expect(make()()).toBe(true);
  });

  test("an explicit constructor calling super() constructs", () => {
    const make = () => {
      let C = class { constructor() { this.base = true; } };
      { C = class extends C { constructor() { super(); this.sub = true; } }; }
      return () => new C();
    };
    const instance = make()();
    expect(instance.base).toBe(true);
    expect(instance.sub).toBe(true);
  });

  test("methods and static fields that use super see the superclass", () => {
    const make = () => {
      let C = class {
        static tag = "a";
        m() { return "base"; }
      };
      { C = class extends C {
        static tag2 = super.tag + "b";
        m() { return "sub>" + super.m(); }
      }; }
      return () => [C.tag2, new C().m()];
    };
    expect(make()()).toEqual(["ab", "sub>base"]);
  });

  test("locals declared after the block keep their own values", () => {
    const make = () => {
      let a = "a";
      { const K = class extends Object {}; }
      let b = "b";
      let c = "c";
      return () => [a, b, c];
    };
    expect(make()()).toEqual(["a", "b", "c"]);
  });

  test("locals after a switch case or try block that evaluates the class keep their values", () => {
    class Base {}
    const inSwitch = () => {
      let a = "a";
      switch (a) { case "a": { const K = class extends Base {}; } }
      let b = "b";
      let c = "c";
      return () => [a, b, c];
    };
    const inTry = () => {
      let a = "a";
      try { const K = class extends Base {}; } catch { }
      let b = "b";
      let c = "c";
      return () => [a, b, c];
    };
    expect(inSwitch()()).toEqual(["a", "b", "c"]);
    expect(inTry()()).toEqual(["a", "b", "c"]);
  });

  test("two anonymous derived classes in one block", () => {
    class Base { static v = "base"; }
    const make = () => {
      let a = "a";
      let pair;
      { pair = [class extends Base {}, class extends Base {}]; }
      let b = "b";
      let c = "c";
      return () => [a, b, c, pair[1].v];
    };
    expect(make()()).toEqual(["a", "b", "c", "base"]);
  });

  test("the reassignment in doubly nested blocks", () => {
    const make = () => {
      let C = class {};
      { { C = class extends C {}; } }
      const x = 1;
      return () => () => [typeof new C(), x];
    };
    expect(make()()()).toEqual(["object", 1]);
  });

  test("the heritage reads the outer binding through a call", () => {
    const make = () => {
      let C = class {};
      const first = C;
      const id = (value) => value;
      { C = class extends id(C) {}; }
      return () => [typeof new C(), Object.getPrototypeOf(C) === first];
    };
    expect(make()()).toEqual(["object", true]);
  });

  test("a class expression with its own name", () => {
    const make = () => {
      let C = class {};
      const first = C;
      { C = class D extends C { static self() { return D; } }; }
      const inner = C;
      return () => [C === inner, C.self() === inner, C.name,
        Object.getPrototypeOf(C) === first, typeof new C()];
    };
    expect(make()()).toEqual([true, true, "D", true, "object"]);
  });

  test("a class expression named after the binding it is assigned to reads its own TDZ binding", () => {
    const make = () => {
      let C = class {};
      { C = class C extends C {}; }
      return () => C;
    };
    expect(make).toThrow(ReferenceError);
  });

  test("a class passed as an argument in a block", () => {
    const make = () => {
      let C = class {};
      const id = (value) => value;
      let D;
      { D = id(class extends C {}); }
      const z = "z";
      return () => [D.name, typeof new D(), z, typeof new C()];
    };
    expect(make()()).toEqual(["", "object", "z", "object"]);
  });

  test("a for...of loop body reassigns the binding", () => {
    const make = () => {
      let C = class {};
      const first = C;
      for (const _ of [1, 2]) C = class extends C {};
      return () => [typeof new C(), Object.getPrototypeOf(Object.getPrototypeOf(C)) === first];
    };
    expect(make()()).toEqual(["object", true]);
  });

  test("closures created in each for...of iteration see their own class", () => {
    const fns = [];
    let K = class { id() { return 0; } };
    for (const i of [1, 2, 3]) {
      K = class extends K { id() { return super.id() + i; } };
      const Current = K;
      fns.push(() => new Current().id());
    }
    expect(fns.map((fn) => fn())).toEqual([1, 3, 6]);
  });

  test("a generator closure", () => {
    const make = () => {
      let C = class {};
      { C = class extends C {}; }
      return { *construct() { yield typeof new C(); } };
    };
    expect(make().construct().next().value).toBe("object");
  });

  test("an async closure", async () => {
    const make = () => {
      let C = class {};
      { C = class extends C {}; }
      return async () => typeof new C();
    };
    expect(await make()()).toBe("object");
  });
});
