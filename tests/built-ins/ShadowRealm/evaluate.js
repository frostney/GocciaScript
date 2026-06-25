describe("ShadowRealm.prototype.evaluate", () => {
  test("is a function with name 'evaluate' and length 1", () => {
    expect(typeof ShadowRealm.prototype.evaluate).toBe("function");
    expect(ShadowRealm.prototype.evaluate.name).toBe("evaluate");
    expect(ShadowRealm.prototype.evaluate.length).toBe(1);
  });

  test("is not a constructor", () => {
    expect(() => new ShadowRealm.prototype.evaluate()).toThrow(TypeError);
  });

  test("returns primitive results unchanged", () => {
    const realm = new ShadowRealm();
    expect(realm.evaluate("1 + 1")).toBe(2);
    expect(realm.evaluate("'a' + 'b'")).toBe("ab");
    expect(realm.evaluate("true")).toBe(true);
    expect(realm.evaluate("null")).toBe(null);
    expect(realm.evaluate("undefined")).toBe(undefined);
    expect(realm.evaluate("10n")).toBe(10n);
    expect(typeof realm.evaluate("Symbol('x')")).toBe("symbol");
  });

  test("an empty body returns undefined", () => {
    expect(new ShadowRealm().evaluate("")).toBe(undefined);
  });

  test("has access to standard intrinsics inside the realm", () => {
    const realm = new ShadowRealm();
    expect(realm.evaluate("typeof Object")).toBe("function");
    expect(realm.evaluate("[1, 2, 3].map((x) => x * 2).join(',')")).toBe("2,4,6");
  });

  test("throws a TypeError when the source is not a string", () => {
    const realm = new ShadowRealm();
    expect(() => realm.evaluate(123)).toThrow(TypeError);
    expect(() => realm.evaluate({})).toThrow(TypeError);
    expect(() => realm.evaluate()).toThrow(TypeError);
  });

  test("throws a TypeError on an incompatible receiver", () => {
    expect(() => ShadowRealm.prototype.evaluate.call({}, "1")).toThrow(TypeError);
  });

  test("wraps an error thrown inside the realm as a caller-realm TypeError", () => {
    const realm = new ShadowRealm();
    expect(() => realm.evaluate("throw new RangeError('inner')")).toThrow(
      TypeError,
    );
  });

  test("throws a SyntaxError when the source does not parse", () => {
    const realm = new ShadowRealm();
    expect(() => realm.evaluate(")(")).toThrow(SyntaxError);
  });

  test("throws a SyntaxError when a top-level lexical binding is declared twice", () => {
    const realm = new ShadowRealm();
    expect(() => realm.evaluate("let x; let x;")).toThrow(SyntaxError);
    expect(() => realm.evaluate("const y = 1; const y = 2;")).toThrow(
      SyntaxError,
    );
    expect(() => realm.evaluate("let z; const z = 1;")).toThrow(SyntaxError);
    expect(() => realm.evaluate("class C {} class C {}")).toThrow(SyntaxError);
    // A declaration conflict is a pre-execution early error, so it stays a
    // SyntaxError rather than being wrapped as the TypeError used for runtime
    // abrupt completions; re-evaluating a fresh lexical name still succeeds.
    expect(realm.evaluate("const ok = 2; ok")).toBe(2);
  });

  test("throws a TypeError when the result is a non-callable object", () => {
    const realm = new ShadowRealm();
    expect(() => realm.evaluate("({})")).toThrow(TypeError);
    expect(() => realm.evaluate("[1, 2, 3]")).toThrow(TypeError);
  });

  test("keeps mutations isolated from the caller realm", () => {
    const realm = new ShadowRealm();
    realm.evaluate("globalThis.shared = 42");
    expect(realm.evaluate("globalThis.shared")).toBe(42);
    expect(typeof globalThis.shared).toBe("undefined");
  });

  test("keeps separate ShadowRealms isolated from each other", () => {
    const a = new ShadowRealm();
    const b = new ShadowRealm();
    a.evaluate("globalThis.token = 1");
    expect(b.evaluate("typeof globalThis.token")).toBe("undefined");
  });

  test("retains realm state across evaluate calls", () => {
    const realm = new ShadowRealm();
    realm.evaluate("globalThis.count = 0");
    realm.evaluate("globalThis.count += 1");
    realm.evaluate("globalThis.count += 1");
    expect(realm.evaluate("globalThis.count")).toBe(2);
  });

  test("can create and use a nested ShadowRealm", () => {
    const outer = new ShadowRealm();
    expect(
      outer.evaluate(
        "const inner = new ShadowRealm(); inner.evaluate('2 + 3')",
      ),
    ).toBe(5);
  });

  test("persists var and function declarations across evaluate calls", () => {
    const realm = new ShadowRealm();
    realm.evaluate("var stored = 7; function makeNine() { return 9; }");
    expect(realm.evaluate("stored")).toBe(7);
    expect(realm.evaluate("makeNine()")).toBe(9);
  });

  test("surfaces top-level var and function declarations on the realm globalThis", () => {
    const realm = new ShadowRealm();
    realm.evaluate("var onGlobal = 11; function fnGlobal() {}");
    expect(realm.evaluate("globalThis.onGlobal")).toBe(11);
    expect(realm.evaluate("typeof globalThis.fnGlobal")).toBe("function");
  });

  test("does not persist top-level let or const across evaluate calls", () => {
    const realm = new ShadowRealm();
    realm.evaluate("const scoped = 1");
    expect(realm.evaluate("typeof scoped")).toBe("undefined");
    // Re-declaring the same top-level lexical name must not clash.
    expect(realm.evaluate("const scoped = 2; scoped")).toBe(2);
  });
});

describe("ShadowRealm wrapped functions", () => {
  test("returns a callable wrapped function for a callable result", () => {
    const realm = new ShadowRealm();
    const add = realm.evaluate("(a, b) => a + b");
    expect(typeof add).toBe("function");
    expect(add(2, 3)).toBe(5);
  });

  test("copies the target's name and length onto the wrapped function", () => {
    const realm = new ShadowRealm();
    const fn = realm.evaluate("const tagged = (a, b, c) => a; tagged");
    expect(fn.name).toBe("tagged");
    expect(fn.length).toBe(3);
  });

  test("marshals a callable argument into the realm and its result back out", () => {
    const realm = new ShadowRealm();
    const apply = realm.evaluate("(callback) => callback(20) + 1");
    expect(apply((n) => n * 2)).toBe(41);
  });

  test("wraps a function returned by a wrapped function", () => {
    const realm = new ShadowRealm();
    const makeAdder = realm.evaluate("(a) => (b) => a + b");
    const addFive = makeAdder(5);
    expect(typeof addFive).toBe("function");
    expect(addFive(3)).toBe(8);
  });

  test("wraps an error thrown by a wrapped function as a caller-realm TypeError", () => {
    const realm = new ShadowRealm();
    const boom = realm.evaluate("() => { throw new Error('boom'); }");
    expect(() => boom()).toThrow(TypeError);
  });

  test("throws a TypeError when a wrapped function returns a non-callable object", () => {
    const realm = new ShadowRealm();
    const makeObject = realm.evaluate("() => ({})");
    expect(() => makeObject()).toThrow(TypeError);
  });

  test("a wrapped function is not a constructor", () => {
    const realm = new ShadowRealm();
    const fn = realm.evaluate("(x) => x");
    expect(() => new fn()).toThrow(TypeError);
  });
});

describe("ShadowRealm evaluate conformance", () => {
  test("shares the global symbol registry with the caller realm", () => {
    const realm = new ShadowRealm();
    const shadowSym = realm.evaluate('Symbol.for("shadowrealm-shared-key")');
    expect(typeof shadowSym).toBe("symbol");
    expect(shadowSym).toBe(Symbol.for("shadowrealm-shared-key"));
    expect(Symbol.keyFor(shadowSym)).toBe("shadowrealm-shared-key");
  });

  test("a symbol created with Symbol() in the realm is not in the shared registry", () => {
    const realm = new ShadowRealm();
    const shadowSym = realm.evaluate('Symbol("not-registered")');
    expect(Symbol.keyFor(shadowSym)).toBe(undefined);
  });

  test("throws a SyntaxError for a strict-mode reserved word in the body", () => {
    const realm = new ShadowRealm();
    expect(() => realm.evaluate('"use strict"; var public = 1;')).toThrow(
      SyntaxError,
    );
  });

  test("wraps a runtime error thrown inside the realm as a caller-realm TypeError", () => {
    const realm = new ShadowRealm();
    expect(() => realm.evaluate("throw new SyntaxError('runtime')")).toThrow(
      TypeError,
    );
  });

  test("copies the target function length onto the wrapped function", () => {
    const realm = new ShadowRealm();
    expect(realm.evaluate("(a, b, c) => {}").length).toBe(3);
  });

  test("preserves an infinite target length on the wrapped function", () => {
    const realm = new ShadowRealm();
    const fn = realm.evaluate(
      "const f = () => {}; Object.defineProperty(f, 'length', { get: () => Infinity, configurable: true }); f;",
    );
    expect(fn.length).toBe(Infinity);
  });

  test("does not expose eval in the child realm when the host has none", () => {
    const realm = new ShadowRealm();
    expect(realm.evaluate("typeof eval")).toBe("undefined");
  });
});
