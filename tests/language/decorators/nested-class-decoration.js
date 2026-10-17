/*---
description: a class defined and decorated while another class is being decorated keeps its own decoration state, and the other class's decoration continues unaffected
features: [decorators, decorator-metadata, auto-accessor, class-static-block, generators, Goccia.gc]
---*/

// Decorators proposal (tc39/ecma262 PR #2417), ClassDefinitionEvaluation:
// each evaluation of a class has its own metadata object and extra
// initializer lists and applies its own decorators. A decorator is ordinary
// code, so it can evaluate another decorated class while its own class is
// being decorated.

const collectGarbage = () => {
  if (typeof Goccia !== "undefined") {
    Goccia.gc();
  }
};

// A method decorator that records its initializer and collects first.
const recordingDecorator = (log, tag) => (value, context) => {
  collectGarbage();
  context.addInitializer(() => {
    collectGarbage();
    log.push(tag + "-init");
  });
};

describe("a decorated class defined inside a decorator", () => {
  test("the outer class's remaining element decorators and initializers still run", () => {
    const log = [];
    const inner = recordingDecorator(log, "inner");
    const outer = (value, context) => {
      @inner class I {}
      collectGarbage();
      context.addInitializer(() => { log.push("outer-init"); });
    };
    const outer2 = recordingDecorator(log, "outer2");
    class O { @outer m() {} @outer2 n() {} }
    new O();
    expect(log.join(",")).toBe("inner-init,outer-init,outer2-init");
  });

  test("a class decorator's replacement class is kept", () => {
    const inner = (value, context) => { collectGarbage(); };
    const outer = (C, context) => {
      @inner class I {}
      collectGarbage();
      return class extends C { static tag = "replaced"; };
    };
    @outer class O {}
    expect(O.tag).toBe("replaced");
    expect(Object.getPrototypeOf(O).name).toBe("O");
  });

  test("three levels of nesting each keep their own decoration", () => {
    const log = [];
    const level3 = (C, context) => {
      context.metadata.level = 3;
      context.addInitializer(() => { log.push("c3"); });
      collectGarbage();
    };
    let Level3;
    const level2 = (value, context) => {
      context.metadata.level = 2;
      @level3 class L3 {}
      Level3 = L3;
      context.addInitializer(() => { log.push("m2"); });
    };
    let Level2;
    const level1 = (C, context) => {
      context.metadata.level = 1;
      class L2 { @level2 m() {} }
      Level2 = L2;
      new L2();
      context.addInitializer(() => { log.push("c1"); });
      return class extends C { static wrapped = true; };
    };
    @level1 class L1 {}
    expect(log.join(",")).toBe("c3,m2,c1");
    expect(L1.wrapped).toBe(true);
    expect(L1[Symbol.metadata].level).toBe(1);
    expect(Level2[Symbol.metadata].level).toBe(2);
    expect(Level3[Symbol.metadata].level).toBe(3);
  });

  test("class and element decorators on both classes add initializers and metadata to their own class", () => {
    const log = [];
    const tagged = (name) => (value, context) => {
      context.metadata[context.kind + ":" + String(context.name)] = name;
      context.addInitializer(() => { log.push(name + ":" + context.kind + ":" + String(context.name)); });
    };
    let Inner;
    const innerClassDecorator = tagged("inner");
    const outerMethod = (value, context) => {
      @innerClassDecorator
      class I {
        @(tagged("inner")) static s() {}
        @(tagged("inner")) f = 1;
        @(tagged("inner")) accessor a = 2;
        @(tagged("inner")) get g() { return 3; }
      }
      Inner = I;
      new I();
      tagged("outer")(value, context);
    };
    @(tagged("outer"))
    class O {
      @outerMethod m() {}
      @(tagged("outer")) static t() {}
      @(tagged("outer")) x = 4;
    }
    new O();
    // The inner class is defined and constructed while the outer class's
    // decorators run, so all of its initializers run first. Within a class,
    // sorting leaves out when static method initializers run relative to
    // class initializers, which this test does not cover.
    expect(log.slice(0, 5).sort().join(",")).toBe(
      "inner:accessor:a,inner:class:I,inner:field:f,inner:getter:g,inner:method:s");
    expect(log.slice(5).sort().join(",")).toBe(
      "outer:class:O,outer:field:x,outer:method:m,outer:method:t");
    expect(Object.keys(O[Symbol.metadata]).sort().join(",")).toBe(
      "class:O,field:x,method:m,method:t");
    expect(Object.keys(Inner[Symbol.metadata]).sort().join(",")).toBe(
      "accessor:a,class:I,field:f,getter:g,method:s");
  });

  test("an inner decoration that throws and is caught leaves the outer class's decoration intact", () => {
    const log = [];
    const bad = () => { throw new Error("inner"); };
    const outer = (value, context) => {
      try {
        @bad class I {}
      } catch (e) {
        log.push("caught:" + e.message);
      }
      collectGarbage();
      context.addInitializer(() => { log.push("outer-init"); });
    };
    const outer2 = (value, context) => {
      log.push("outer2:" + context.name);
      context.addInitializer(() => { log.push("outer2-init"); });
    };
    const classDecorator = (C, context) => {
      context.metadata.k = 1;
      log.push("class:" + context.name);
    };
    @classDecorator class O { @outer m() {} @outer2 n() {} }
    new O();
    expect(log.join(",")).toBe(
      "caught:inner,outer2:n,class:O,outer-init,outer2-init");
    expect(O[Symbol.metadata].k).toBe(1);
  });

  test("an element decorator of the inner class that throws and is caught leaves the outer class's decoration intact", () => {
    const log = [];
    const bad = () => { throw new TypeError("element"); };
    const outer = (C, context) => {
      try {
        class I { @bad m() {} }
      } catch (e) {
        log.push("caught:" + e.message);
      }
      collectGarbage();
      context.addInitializer(() => { log.push("outer-init"); });
      return class extends C { static replaced = true; };
    };
    const outer2 = (C, context) => {
      log.push("outer2:" + context.name);
    };
    @outer @outer2 class O {}
    expect(log.join(",")).toBe("outer2:O,caught:element,outer-init");
    expect(O.replaced).toBe(true);
  });

  test("a decorated class defined in a static block inside a decorator", () => {
    const log = [];
    const inner = recordingDecorator(log, "inner");
    let Defined;
    const outer = (value, context) => {
      class Holder {
        static {
          collectGarbage();
          class I { @inner m() {} }
          new I();
          Defined = I;
        }
      }
      context.addInitializer(() => { log.push("outer-init"); });
    };
    const outer2 = recordingDecorator(log, "outer2");
    class O { @outer m() {} @outer2 n() {} }
    new O();
    expect(log.join(",")).toBe("inner-init,outer-init,outer2-init");
    expect(typeof Defined).toBe("function");
  });

  test("a decorator inside a static block decorates its own class, not the class the block belongs to", () => {
    const log = [];
    const inner = (C, context) => {
      context.metadata.inner = true;
      context.addInitializer(() => { log.push("inner:" + context.name); });
    };
    let Inner;
    const outer = (C, context) => {
      context.metadata.outer = true;
      context.addInitializer(() => { log.push("outer:" + context.name); });
    };
    @outer class O {
      static {
        @inner class I {}
        Inner = I;
      }
    }
    expect(log.join(",")).toBe("inner:I,outer:O");
    expect(O[Symbol.metadata].inner).toBe(undefined);
    expect(Inner[Symbol.metadata].outer).toBe(undefined);
  });

  test("a decorated class expression inside a decorator expression", () => {
    const log = [];
    const inner = recordingDecorator(log, "inner");
    const outer2 = recordingDecorator(log, "outer2");
    let Expr;
    class O {
      @((Expr = @inner class { @inner x() {} }), recordingDecorator(log, "outer")) m() {}
      @outer2 n() {}
    }
    new Expr();
    new O();
    expect(log.join(",")).toBe("inner-init,inner-init,outer-init,outer2-init");
  });

  test("a private auto-accessor of a class defined inside a decorator belongs to that class", () => {
    let Inner;
    const outer = (value, context) => {
      class I {
        accessor #x = 5;
        static read(o) { return o.#x; }
        static has(o) { return #x in o; }
      }
      Inner = I;
    };
    class O { @outer m() {} }
    const i = new Inner();
    expect(Inner.read(i)).toBe(5);
    expect(Inner.has(i)).toBe(true);
    expect(Inner.has(new O())).toBe(false);
  });

  test("many nested decorations under collection keep each class's state", () => {
    const log = [];
    const inner = (C, context) => {
      collectGarbage();
      context.addInitializer(() => { log.push("i"); });
      return class extends C { static inner = true; };
    };
    const classes = [];
    const outer = (value, context) => {
      for (const i of Array.from({ length: 20 }, (_, n) => n)) {
        @inner class I {}
        classes.push(I);
      }
      context.addInitializer(() => { log.push("o"); });
    };
    class O { @outer m() {} @outer n() {} }
    new O();
    expect(classes.length).toBe(40);
    expect(classes.every((C) => C.inner === true)).toBe(true);
    expect(log.filter((e) => e === "i").length).toBe(40);
    expect(log.slice(-2).join(",")).toBe("o,o");
  });
});

describe("decoration across a suspension", () => {
  test("a generator that defines a decorated class at each step, stepped inside a decorator", () => {
    const log = [];
    const inner = recordingDecorator(log, "inner");
    const host = {
      *steps() {
        @inner class A {}
        yield A;
        @inner class B {}
        yield B;
      },
    };
    const it = host.steps();
    const outer = (value, context) => {
      it.next();
      collectGarbage();
      context.addInitializer(() => { log.push("outer-init"); });
    };
    const outer2 = (value, context) => {
      it.next();
      context.addInitializer(() => { log.push("outer2-init"); });
    };
    class O { @outer m() {} @outer2 n() {} }
    new O();
    expect(log.join(",")).toBe("inner-init,inner-init,outer-init,outer2-init");
  });
});
