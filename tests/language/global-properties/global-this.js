/*---
description: >
  globalThis is a global property per ECMAScript spec.
  It is an object whose properties reflect the built-in globals.
  globalThis.globalThis === globalThis (self-referential).
features: [global-properties, globalThis]
---*/

describe("globalThis basics", () => {
  test("globalThis is an object", () => {
    expect(typeof globalThis).toBe("object");
  });

  test("globalThis is self-referential", () => {
    expect(globalThis.globalThis === globalThis).toBe(true);
    expect(globalThis.globalThis.globalThis === globalThis).toBe(true);
  });
});

describe("globalThis exposes built-in values", () => {
  test("undefined", () => {
    expect(globalThis.undefined).toBe(undefined);
  });

  test("NaN", () => {
    expect(Number.isNaN(globalThis.NaN)).toBe(true);
  });

  test("Infinity", () => {
    expect(globalThis.Infinity).toBe(Infinity);
  });
});

describe("globalThis exposes built-in constructors", () => {
  test("Object", () => {
    expect(typeof globalThis.Object).toBe("function");
  });

  test("Array", () => {
    expect(typeof globalThis.Array).toBe("function");
  });

  test("String", () => {
    expect(typeof globalThis.String).toBe("function");
  });

  test("Number", () => {
    expect(typeof globalThis.Number).toBe("function");
  });

  test("Boolean", () => {
    expect(typeof globalThis.Boolean).toBe("function");
  });

  test("Function", () => {
    expect(typeof globalThis.Function).toBe("function");
  });

  test("Symbol", () => {
    expect(typeof globalThis.Symbol).toBe("function");
  });

  test("Set", () => {
    expect(typeof globalThis.Set).toBe("function");
  });

  test("Map", () => {
    expect(typeof globalThis.Map).toBe("function");
  });

  test("Promise", () => {
    expect(typeof globalThis.Promise).toBe("function");
  });
});

describe("globalThis exposes error constructors", () => {
  test("Error", () => {
    expect(typeof globalThis.Error).toBe("function");
  });

  test("TypeError", () => {
    expect(typeof globalThis.TypeError).toBe("function");
  });

  test("ReferenceError", () => {
    expect(typeof globalThis.ReferenceError).toBe("function");
  });

  test("RangeError", () => {
    expect(typeof globalThis.RangeError).toBe("function");
  });
});

describe("globalThis exposes namespace objects", () => {
  test("console", () => {
    expect(typeof globalThis.console).toBe("object");
  });

  test("Math", () => {
    expect(typeof globalThis.Math).toBe("object");
  });

  test("JSON", () => {
    expect(typeof globalThis.JSON).toBe("object");
  });

  const hasTemporal = typeof Temporal !== "undefined";

  test.runIf(hasTemporal)("Temporal", () => {
    expect(typeof globalThis.Temporal).toBe("object");
  });
});

describe("globalThis exposes utility functions", () => {
  test("queueMicrotask", () => {
    expect(typeof globalThis.queueMicrotask).toBe("function");
  });
});

describe("globalThis constructor identity", () => {
  test("globalThis.Object is the same as Object", () => {
    expect(globalThis.Object === Object).toBe(true);
  });

  test("globalThis.Array is the same as Array", () => {
    expect(globalThis.Array === Array).toBe(true);
  });

  test("globalThis.Math is the same as Math", () => {
    expect(globalThis.Math === Math).toBe(true);
  });

  test("globalThis.JSON is the same as JSON", () => {
    expect(globalThis.JSON === JSON).toBe(true);
  });
});

describe("globalThis immutability", () => {
  test("global globalThis cannot be reassigned", () => {
    expect(() => {
      globalThis = {};
    }).toThrow();
  });

  test("global globalThis remains unchanged after failed reassignment", () => {
    try {
      globalThis = {};
    } catch (e) {
      // expected
    }
    expect(typeof globalThis).toBe("object");
    expect(globalThis.globalThis === globalThis).toBe(true);
  });
});

describe("globalThis shadowing", () => {
  test("globalThis can be shadowed in a block scope with let", () => {
    expect(typeof globalThis).toBe("object");
    {
      let globalThis = 42;
      expect(globalThis).toBe(42);
    }
    expect(typeof globalThis).toBe("object");
  });

  test("globalThis can be shadowed in a block scope with const", () => {
    {
      const globalThis = "shadowed";
      expect(globalThis).toBe("shadowed");
    }
    expect(typeof globalThis).toBe("object");
  });

  test("shadowed globalThis does not affect outer scope", () => {
    const outer = globalThis;
    {
      let globalThis = "local";
      expect(globalThis).toBe("local");
    }
    expect(outer === globalThis).toBe(true);
  });

  test("arrow function parameter can shadow globalThis", () => {
    const fn = (globalThis) => globalThis;
    expect(fn(42)).toBe(42);
    expect(fn("test")).toBe("test");
  });
});
