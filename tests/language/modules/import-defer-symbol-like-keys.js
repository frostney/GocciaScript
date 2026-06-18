import defer * as ns from "./helpers/deferred-symbol-like-target.js";
import defer * as thenNs from "./helpers/deferred-then-target.js";

describe("import defer symbol-like namespace keys", () => {
  test("symbol and private-brand operations do not evaluate deferred modules", () => {
    globalThis.__gocciaImportDeferSymbolLikeEvents = [];

    expect(ns[Symbol()]).toBeUndefined();
    expect(ns[Symbol.toStringTag]).toBe("Deferred Module");
    expect(Object.getOwnPropertyDescriptor(ns, Symbol.toStringTag).value).toBe(
      "Deferred Module",
    );

    try {
      Object.defineProperty(ns, Symbol(), { value: "quiet" });
    } catch (_) {}

    class A {
      constructor() {
        return ns;
      }
    }

    class B extends A {
      [Symbol()] = 10;
    }

    try {
      new B();
    } catch (_) {}

    class Returner {
      constructor(value) {
        return value;
      }
    }

    class Marker extends Returner {
      #mark = "marked";

      static mark(value) {
        new Marker(value);
      }

      static hasMark(value) {
        return #mark in value;
      }
    }

    expect(() => Marker.mark(ns)).toThrow(TypeError);
    expect(Marker.hasMark(ns)).toBe(false);
    expect(globalThis.__gocciaImportDeferSymbolLikeEvents).toEqual([]);

    expect(ns.exported).toBe(41);
    expect(globalThis.__gocciaImportDeferSymbolLikeEvents).toEqual(["target"]);
  });

  test("then stays ordinary even after another export evaluates the module", () => {
    globalThis.__gocciaImportDeferThenEvents = [];

    expect(thenNs.then).toBeUndefined();
    expect(globalThis.__gocciaImportDeferThenEvents).toEqual([]);

    expect(thenNs.exported).toBe(42);
    expect(globalThis.__gocciaImportDeferThenEvents).toEqual(["then-target"]);
    expect(thenNs.then).toBeUndefined();
  });
});
