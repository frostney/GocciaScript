describe("Symbol.dispose", () => {
  test("is a well-known symbol", () => {
    expect(typeof Symbol.dispose).toBe("symbol");
  });

  test("has description 'Symbol.dispose'", () => {
    expect(Symbol.dispose.toString()).toBe("Symbol(Symbol.dispose)");
  });

  test("is the same reference on every access", () => {
    expect(Symbol.dispose).toBe(Symbol.dispose);
  });
});

describe("Symbol.asyncDispose", () => {
  test("is a well-known symbol", () => {
    expect(typeof Symbol.asyncDispose).toBe("symbol");
  });

  test("has description 'Symbol.asyncDispose'", () => {
    expect(Symbol.asyncDispose.toString()).toBe("Symbol(Symbol.asyncDispose)");
  });

  test("is the same reference on every access", () => {
    expect(Symbol.asyncDispose).toBe(Symbol.asyncDispose);
  });

  test("Symbol.dispose and Symbol.asyncDispose are different", () => {
    expect(Symbol.dispose).not.toBe(Symbol.asyncDispose);
  });
});
