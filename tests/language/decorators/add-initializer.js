/*---
description: addInitializer on decorator context runs callbacks at the correct time
features: [decorators, Proxy, Goccia.gc]
---*/

const collectGarbage = () => {
  if (typeof Goccia !== "undefined") {
    Goccia.gc();
  }
};

// Each entry defines a class whose one decorator keeps its context, and
// returns that context.
const decoratedElements = {
  class: () => {
    let saved;
    const keep = (value, context) => { saved = context; };
    @keep class C {}
    return saved;
  },
  method: () => {
    let saved;
    const keep = (value, context) => { saved = context; };
    class C { @keep m() {} }
    return saved;
  },
  getter: () => {
    let saved;
    const keep = (value, context) => { saved = context; };
    class C { @keep get g() { return 1; } }
    return saved;
  },
  setter: () => {
    let saved;
    const keep = (value, context) => { saved = context; };
    class C { @keep set s(v) {} }
    return saved;
  },
  field: () => {
    let saved;
    const keep = (value, context) => { saved = context; };
    class C { @keep x = 1; }
    return saved;
  },
  accessor: () => {
    let saved;
    const keep = (value, context) => { saved = context; };
    class C { @keep accessor a = 1; }
    return saved;
  },
  "static method": () => {
    let saved;
    const keep = (value, context) => { saved = context; };
    class C { @keep static m() {} }
    return saved;
  },
  "static field": () => {
    let saved;
    const keep = (value, context) => { saved = context; };
    class C { @keep static x = 1; }
    return saved;
  },
  "static accessor": () => {
    let saved;
    const keep = (value, context) => { saved = context; };
    class C { @keep static accessor a = 1; }
    return saved;
  },
  "private method": () => {
    let saved;
    const keep = (value, context) => { saved = context; };
    class C { @keep #m() {} }
    return saved;
  },
  "private getter": () => {
    let saved;
    const keep = (value, context) => { saved = context; };
    class C { @keep get #g() { return 1; } }
    return saved;
  },
  "private field": () => {
    let saved;
    const keep = (value, context) => { saved = context; };
    class C { @keep #x = 1; }
    return saved;
  },
  "static private method": () => {
    let saved;
    const keep = (value, context) => { saved = context; };
    class C { @keep static #m() {} }
    return saved;
  },
};

describe("addInitializer", () => {
  test("class addInitializer runs after class definition", () => {
    let initialized = false;

    const init = (cls, context) => {
      context.addInitializer(() => {
        initialized = true;
      });
    };

    expect(initialized).toBe(false);

    @init
    class C {}

    expect(initialized).toBe(true);
  });

  test("method addInitializer runs during construction", () => {
    let initCalled = false;

    const dec = (method, context) => {
      context.addInitializer(() => {
        initCalled = true;
      });
    };

    class C {
      @dec
      foo() {}
    }

    expect(initCalled).toBe(false);
    const c = new C();
    expect(initCalled).toBe(true);
  });
});

describe("addInitializer after the decorator has returned", () => {
  for (const kind of Object.keys(decoratedElements)) {
    test(`throws a TypeError for a ${kind} decorator`, () => {
      const context = decoratedElements[kind]();
      expect(() => context.addInitializer(() => {})).toThrow(TypeError);
    });

    test(`throws a TypeError for a ${kind} decorator after a collection`, () => {
      const context = decoratedElements[kind]();
      const addInitializer = context.addInitializer;
      collectGarbage();
      expect(() => addInitializer(() => {})).toThrow(TypeError);
      collectGarbage();
      expect(() => context.addInitializer(() => {})).toThrow(TypeError);
    });
  }

  test("checks that decoration has finished before checking the argument", () => {
    const context = decoratedElements.method();
    expect(() => context.addInitializer(42)).toThrow(TypeError);
  });

  test("decorated classes still work after a late call threw", () => {
    const context = decoratedElements.class();
    expect(() => context.addInitializer(() => {})).toThrow(TypeError);
    collectGarbage();

    const log = [];
    const dec = (value, context) => {
      context.addInitializer(() => { log.push(context.kind); });
    };
    @dec class C {
      @dec m() {}
    }
    new C();
    expect(log).toEqual(["class", "method"]);
  });

  test("a later decorator of the same class cannot use an earlier decorator's addInitializer", () => {
    let first;
    let lateCallError;
    const keep = (value, context) => { first = context; };
    const useEarlier = (value, context) => {
      try {
        first.addInitializer(() => {});
      } catch (e) {
        lateCallError = e;
      }
    };

    class C {
      @keep a() {}
      @useEarlier b() {}
    }

    expect(lateCallError).toBeInstanceOf(TypeError);
  });

  test("each decorator of one element gets its own addInitializer", () => {
    let inner;
    let outerCallError = "none";
    const keepInner = (value, context) => { inner = context; };
    const useInner = (value, context) => {
      expect(context.addInitializer).not.toBe(inner.addInitializer);
      try {
        inner.addInitializer(() => {});
      } catch (e) {
        outerCallError = e;
      }
    };

    class C {
      @useInner @keepInner m() {}
    }

    expect(outerCallError).toBeInstanceOf(TypeError);
  });

  test("works while the decorator runs even after a collection", () => {
    let ran = false;
    const dec = (value, context) => {
      collectGarbage();
      context.addInitializer(() => { ran = true; });
    };

    @dec class C {}

    expect(ran).toBe(true);
  });

  test("throws after the decorator deleted it from the context and a collection ran", () => {
    let addInitializer;
    const dec = (value, context) => {
      addInitializer = context.addInitializer;
      delete context.addInitializer;
      collectGarbage();
    };

    class C { @dec m() {} }

    collectGarbage();
    expect(() => addInitializer(() => {})).toThrow(TypeError);
  });

  test("keeps the initializer list alive for a decorator that threw", () => {
    let saved;
    const failing = (value, context) => {
      saved = context;
      throw new RangeError("decorator failed");
    };

    expect(() => {
      class C { @failing m() {} }
    }).toThrow(RangeError);

    collectGarbage();
    // The decorator did not return, so its decoration never finished.
    expect(saved.addInitializer(() => {})).toBeUndefined();
  });
});

describe("addInitializer argument", () => {
  const nonCallables = {
    number: 42,
    string: "x",
    object: {},
    undefined: undefined,
    null: null,
    symbol: Symbol("s"),
  };

  for (const label of Object.keys(nonCallables)) {
    test(`throws a TypeError for a ${label}`, () => {
      const dec = (value, context) => {
        context.addInitializer(nonCallables[label]);
      };
      expect(() => {
        class C { @dec m() {} }
      }).toThrow(TypeError);
    });
  }

  test("throws a TypeError when called without an argument", () => {
    const dec = (value, context) => {
      context.addInitializer();
    };
    expect(() => {
      @dec class C {}
    }).toThrow(TypeError);
  });

  test("throws a TypeError for every element kind", () => {
    const dec = (value, context) => {
      context.addInitializer({});
    };
    expect(() => { @dec class C {} }).toThrow(TypeError);
    expect(() => { class C { @dec m() {} } }).toThrow(TypeError);
    expect(() => { class C { @dec get g() { return 1; } } }).toThrow(TypeError);
    expect(() => { class C { @dec set s(v) {} } }).toThrow(TypeError);
    expect(() => { class C { @dec x = 1; } }).toThrow(TypeError);
    expect(() => { class C { @dec accessor a = 1; } }).toThrow(TypeError);
    expect(() => { class C { @dec static m() {} } }).toThrow(TypeError);
    expect(() => { class C { @dec static x = 1; } }).toThrow(TypeError);
    expect(() => { class C { @dec #m() {} } }).toThrow(TypeError);
    expect(() => { class C { @dec #x = 1; } }).toThrow(TypeError);
  });

  test("the decorator can catch the TypeError and still add a function", () => {
    let caught;
    let ran = false;
    const dec = (value, context) => {
      try {
        context.addInitializer(1);
      } catch (e) {
        caught = e;
      }
      context.addInitializer(() => { ran = true; });
    };

    @dec class C {}

    expect(caught).toBeInstanceOf(TypeError);
    expect(ran).toBe(true);
  });

  test("accepts a callable Proxy", () => {
    const calls = [];
    const target = () => { calls.push("called"); };
    const initializer = new Proxy(target, {});
    const dec = (value, context) => {
      context.addInitializer(initializer);
    };

    class C { @dec m() {} }

    expect(calls).toEqual([]);
    new C();
    expect(calls).toEqual(["called"]);
  });

  test("accepts a class, which throws a TypeError when the initializer runs", () => {
    const dec = (value, context) => {
      context.addInitializer(class Initializer {});
    };

    class C { @dec m() {} }

    expect(() => new C()).toThrow(TypeError);
    expect(() => {
      @dec class D {}
    }).toThrow(TypeError);
  });

  test("accepts a Proxy with an apply trap and calls it with the class as this", () => {
    let receiver;
    const initializer = new Proxy(() => {}, {
      apply(target, thisArg, args) {
        receiver = thisArg;
      },
    });
    const dec = (value, context) => {
      context.addInitializer(initializer);
    };

    @dec class C {}

    expect(receiver).toBe(C);
  });
});
