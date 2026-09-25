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

  test("tries the whole path as a literal own key first", () => {
    // Vitest is the oracle: it checks hasOwnProperty(actual, path) before
    // splitting, so a key that literally contains the separator is found.
    expect({ "a.b": 5 }).toHaveProperty("a.b", 5);
    expect({ "a[0]": 5 }).toHaveProperty("a[0]", 5);
    expect({ "x.y.z": 1 }).toHaveProperty("x.y.z", 1);
  });

  test("prefers the literal key over a path that also resolves", () => {
    // The literal check runs first, so it wins outright rather than acting as
    // a fallback: "a.b" reads 5, never the 9 the walk would reach.
    expect({ "a.b": 5, a: { b: 9 } }).toHaveProperty("a.b", 5);
    expect({ "a.b": 5, a: { b: 9 } }).not.toHaveProperty("a.b", 9);
    expect({ "a.b.c": 1, a: { b: {} } }).toHaveProperty("a.b.c", 1);
  });

  test("takes the literal key only as an own member of the whole path", () => {
    // Own, not inherited...
    class DottedPrototypeMember {}
    DottedPrototypeMember.prototype["a.b"] = 5;
    expect(new DottedPrototypeMember()).not.toHaveProperty("a.b", 5);

    // ...but enumerability does not matter...
    const hidden = {};
    Object.defineProperty(hidden, "a.b", { value: 5, enumerable: false });
    expect(hidden).toHaveProperty("a.b", 5);

    // ...and only the whole path is tried, never a segment of it, so a dotted
    // key nested deeper still needs an array path.
    expect({ a: { "b.c": 1 } }).not.toHaveProperty("a.b.c", 1);
    expect({ a: { "b.c": 1 } }).toHaveProperty(["a", "b.c"], 1);
  });

  test("runs an accessor on the literal key exactly once", () => {
    let reads = 0;
    const target = {
      get "a.b"() {
        reads += 1;
        return 5;
      },
    };
    expect(target).toHaveProperty("a.b", 5);
    expect(reads).toBe(1);
  });

  test("uses an array path as the escape hatch for dotted keys", () => {
    expect({ "a.b": 5 }).toHaveProperty(["a.b"], 5);
    expect(nested).toHaveProperty(["a", "b"]);
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
    // Reached as a literal own key, since a string path cannot produce an
    // empty segment.
    expect({ "": 1 }).toHaveProperty("", 1);
  });

  test("drops empty path segments", () => {
    // Vitest is the oracle: its path grammar cannot produce an empty segment,
    // so a leading, doubled or trailing separator is simply not there. "a."
    // resolves to "a" and never reaches an empty-string key.
    expect({ a: 1 }).toHaveProperty("a.", 1);
    expect({ a: 1 }).toHaveProperty("a..", 1);
    expect({ a: 1 }).toHaveProperty(".a", 1);
    expect({ a: { b: 1 } }).toHaveProperty("a..b", 1);
    expect({ a: { b: 1 } }).toHaveProperty("a.");
    expect({ a: { "": 1 } }).not.toHaveProperty("a.", 1);
    expect({ a: { "": 1 } }).toHaveProperty("a.", { "": 1 });
    expect({ a: 1 }).not.toHaveProperty("zz.");
  });

  test("reports a path of nothing but separators as absent", () => {
    // Deliberate divergence, documented in docs/testing-api.md: with every
    // segment dropped there is nothing to resolve, and no own key spells the
    // path literally. Vitest throws a TypeError out of its path parser here
    // ("Cannot read properties of null (reading 'map')"); goccia reports the
    // same verdict as an ordinary assertion failure instead of crashing.
    expect({ "": { "": 1 } }).not.toHaveProperty(".");
    expect({ a: 1 }).not.toHaveProperty("..");
  });

  test("does not unquote bracket segments", () => {
    // Only an array path reaches a key containing a dot.
    expect({ a: { "b.c": 1 } }).not.toHaveProperty('a["b.c"]', 1);
    expect({ a: { "b.c": 1 } }).toHaveProperty(["a", "b.c"], 1);
  });

  test("accepts a number path", () => {
    // Vitest is the oracle. It has no dedicated numeric path support; a number
    // works because its literal own-key check coerces the argument to a
    // property key, so it reaches an index or a numeric key directly.
    expect([1, 2]).toHaveProperty(0, 1);
    expect([1, 2]).toHaveProperty(1);
    expect([1, 2]).not.toHaveProperty(0, 99);
    expect({ 5: "v" }).toHaveProperty(5, "v");
    expect({ "1.5": 1 }).toHaveProperty(1.5, 1);
    expect({ "-1": 1 }).toHaveProperty(-1, 1);
  });

  test("reports a number that matches no key as absent", () => {
    // Deliberate divergence, documented in docs/testing-api.md: once the
    // literal check misses, vitest hands the number to a string-only parser
    // and dies with "path.replace is not a function". Goccia walks it as the
    // path "7" and reports a plain assertion failure.
    expect([1, 2]).not.toHaveProperty(7);
    expect({ a: 1 }).not.toHaveProperty(0);
  });

  test("requires a string, number or array path", () => {
    expect(() => expect({ a: 1 }).toHaveProperty(null)).toThrow();
    expect(() => expect({ a: 1 }).toHaveProperty(undefined)).toThrow();
    expect(() => expect({ a: 1 }).toHaveProperty({})).toThrow();
    expect(() => expect({ a: 1 }).toHaveProperty(true)).toThrow();
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
