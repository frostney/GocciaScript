class WithPrototypeMember {
  constructor() {
    this.own = 1;
  }

  describe() {
    return "prototype member";
  }
}

describe("toHaveProperty", () => {
  const nested = { a: { b: [{ c: 7 }] } };

  test("checks a flat key", () => {
    expect({ a: 1 }).toHaveProperty("a");
    expect({ a: 1 }).not.toHaveProperty("b");
  });

  test("checks a key that exists but holds undefined", () => {
    expect({ k: undefined }).toHaveProperty("k");
    expect({ k: undefined }).toHaveProperty("k", undefined);
  });

  test("walks a dotted path", () => {
    expect(nested).toHaveProperty("a.b");
    expect(nested).not.toHaveProperty("a.zzz");
    expect(nested).not.toHaveProperty("zzz.b");
  });

  test("walks bracket indices", () => {
    expect({ items: [{ type: "x" }] }).toHaveProperty("items[0].type", "x");
    expect([[5]]).toHaveProperty("[0][0]", 5);
    expect({ items: [{ type: "x" }] }).not.toHaveProperty("items[1].type");
  });

  test("indexes arrays with a numeric segment", () => {
    expect({ l: [10, 20] }).toHaveProperty("l.1", 20);
    expect([1, 2]).toHaveProperty("0", 1);
    expect(nested).toHaveProperty("a.b.length", 1);
  });

  test("accepts an array path", () => {
    expect(nested).toHaveProperty(["a", "b", 0, "c"], 7);
    expect(nested).toHaveProperty(["a", "b"]);
    expect(nested).not.toHaveProperty(["a", "b", 1]);
  });

  test("uses an array path as the escape hatch for dotted keys", () => {
    // KNOWN DIVERGENCE (outside the audit corpus, pending a decision):
    // Vitest falls back to the literal key when a dotted path does not
    // resolve, so it finds {"a.b": 5} via the string path "a.b"; goccia
    // always splits on dots.
    expect({ "a.b": 5 }).toHaveProperty(["a.b"], 5);
    expect({ "a.b": 5 }).not.toHaveProperty("a.b", 5);
  });

  test("deep-compares the expected value", () => {
    expect(nested).toHaveProperty("a.b", [{ c: 7 }]);
    expect(nested).not.toHaveProperty("a.b", [{ c: 8 }]);
    expect({ a: { b: 1, c: undefined } }).toHaveProperty("a", { b: 1 });
  });

  test("does not walk through a nullish or missing value", () => {
    // Protected parity with vitest, which reports a plain assertion failure
    // here; bun instead throws a TypeError out of the matcher.
    expect({ a: null }).not.toHaveProperty("a.b");
    expect({ a: undefined }).not.toHaveProperty("a.b");
    expect({}).not.toHaveProperty("zz.b");
  });

  test("reads members of a primitive through its wrapper", () => {
    expect({ a: "xy" }).toHaveProperty("a.length", 2);
    expect({ a: 5 }).not.toHaveProperty("a.b");
  });

  test("resolves inherited members", () => {
    expect(new WithPrototypeMember()).toHaveProperty("own");
    expect(new WithPrototypeMember()).toHaveProperty("describe");
  });

  test("lets a throwing getter mid-path propagate", () => {
    // Protected parity: goccia and vitest agree, bun does not.
    const target = {};
    Object.defineProperty(target, "a", {
      get: () => {
        throw new Error("getter-boom");
      },
      configurable: true,
    });

    expect(() => expect(target).toHaveProperty("a.b")).toThrow("getter-boom");
  });

  test("supports the empty-string key", () => {
    expect({ "": 1 }).toHaveProperty("", 1);
  });

  test("treats a trailing separator as a real empty segment", () => {
    // KNOWN DIVERGENCE (outside the audit corpus, pending a decision):
    // Vitest ignores a trailing separator entirely — "a." resolves to "a" and
    // never reaches an empty-string key.
    expect({ a: 1 }).not.toHaveProperty("a.");
    expect({ a: { b: 1 } }).not.toHaveProperty("a.");
    expect({ a: { "": 1 } }).toHaveProperty("a.", 1);
  });

  test("does not unquote bracket segments", () => {
    // Only an array path reaches a key containing a dot.
    expect({ a: { "b.c": 1 } }).not.toHaveProperty('a["b.c"]', 1);
    expect({ a: { "b.c": 1 } }).toHaveProperty(["a", "b.c"], 1);
  });

  test("requires a string or array path", () => {
    // KNOWN DIVERGENCE (outside the audit corpus, pending a decision):
    // Vitest accepts a numeric path argument rather than raising.
    expect(() => expect([1, 2]).toHaveProperty(0, 1)).toThrow();
    expect(() => expect({ a: 1 }).toHaveProperty(null)).toThrow();
  });

  // The path walk holds each intermediate in a native local only. A getter on a
  // later segment is a GC safe point, so an unrooted intermediate — or the
  // wrapper a primitive segment boxes into — can be collected mid-walk.
  describe.runIf(typeof Goccia !== "undefined")("explicit GC during the walk", () => {
    test("keeps an intermediate alive across a later getter", () => {
      const root = {
        get first() {
          return { second: { get third() { Goccia.gc(); return 7; } } };
        },
      };
      expect(root).toHaveProperty("first.second.third", 7);
    });

    test("keeps a boxed primitive wrapper alive across a getter", () => {
      const root = {
        get text() {
          Goccia.gc();
          return "xy";
        },
      };
      expect(root).toHaveProperty("text.length", 2);
    });

    test("keeps the resolved value alive when the last segment collects", () => {
      const root = {
        outer: {
          get inner() {
            Goccia.gc();
            return { done: true };
          },
        },
      };
      expect(root).toHaveProperty("outer.inner", { done: true });
    });
  });
});
