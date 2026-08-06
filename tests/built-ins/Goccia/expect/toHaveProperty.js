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
    expect({ "a.b": 5 }).toHaveProperty(["a.b"], 5);
    expect({ "a.b": 5 }).not.toHaveProperty("a.b", 5);
  });

  test("deep-compares the expected value", () => {
    expect(nested).toHaveProperty("a.b", [{ c: 7 }]);
    expect(nested).not.toHaveProperty("a.b", [{ c: 8 }]);
    expect({ a: { b: 1, c: undefined } }).toHaveProperty("a", { b: 1 });
  });

  test("does not walk through a nullish or missing value", () => {
    // bun throws a TypeError out of the matcher here; reporting a plain
    // assertion failure keeps .not usable.
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

  test("supports the empty-string key", () => {
    expect({ "": 1 }).toHaveProperty("", 1);
  });

  test("treats a trailing separator as a real empty segment", () => {
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
    expect(() => expect([1, 2]).toHaveProperty(0, 1)).toThrow();
    expect(() => expect({ a: 1 }).toHaveProperty(null)).toThrow();
  });
});
