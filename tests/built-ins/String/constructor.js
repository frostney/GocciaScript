describe("String constructor", () => {
  test("new String() creates wrapper object", () => {
    const s = new String("hello");
    expect(typeof s).toBe("object");
    expect(s.valueOf()).toBe("hello");
  });

  test("new String() has string methods", () => {
    const s = new String("hello");
    expect(s.toUpperCase()).toBe("HELLO");
    expect(s.slice(1, 3)).toBe("el");
    expect(s.includes("ell")).toBe(true);
  });

  test("new String().length works", () => {
    const s = new String("hello");
    expect(s.length).toBe(5);
  });

  test("new String() index access works", () => {
    const s = new String("hello");
    expect(s[0]).toBe("h");
    expect(s[4]).toBe("o");
  });

  test("new String() respects ordinary prototype lookup", () => {
    const ownUndefined = new String("hello");
    ownUndefined.toString = undefined;
    expect(ownUndefined.toString).toBeUndefined();

    const nullPrototype = new String("hello");
    Object.setPrototypeOf(nullPrototype, null);
    expect(nullPrototype.toString).toBeUndefined();
  });

  test("String() as function returns primitive", () => {
    const s = String(42);
    expect(typeof s).toBe("string");
    expect(s).toBe("42");
  });

  test("new String() instanceof String", () => {
    const s = new String("hello");
    expect(s instanceof String).toBe(true);
  });

  test("new String(symbol) wraps display string (ES2026 §22.1.1.1)", () => {
    const s = new String(Symbol("x"));
    expect(typeof s).toBe("object");
    expect(s.valueOf()).toBe("Symbol(x)");
  });

  test("String(obj) invokes user toString() (ES2026 §7.1.17 ToString)", () => {
    class Foo {
      toString() { return "via toString"; }
    }
    expect(String(new Foo())).toBe("via toString");
  });

  test("String(obj) prefers toString() over valueOf() (string hint)", () => {
    const obj = {
      toString() { return "from toString"; },
      valueOf() { return "from valueOf"; },
    };
    expect(String(obj)).toBe("from toString");
  });

  test("String(obj) falls back to valueOf() when toString() returns non-primitive", () => {
    const obj = {
      toString() { return {}; },
      valueOf() { return "fallback"; },
    };
    expect(String(obj)).toBe("fallback");
  });

  test("String(obj) throws TypeError when neither method returns a primitive", () => {
    const obj = {
      toString() { return {}; },
      valueOf() { return {}; },
    };
    expect(() => String(obj)).toThrow(TypeError);
  });

  test("new String(obj) invokes user toString() and wraps the result", () => {
    class Foo {
      toString() { return "wrapped"; }
    }
    const s = new String(new Foo());
    expect(typeof s).toBe("object");
    expect(s.valueOf()).toBe("wrapped");
  });

  test("String(stringWrapper) unwraps via toString()", () => {
    const wrapped = new String("hi");
    expect(String(wrapped)).toBe("hi");
  });
});
