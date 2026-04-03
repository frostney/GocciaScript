describe("Well-known Symbols", () => {
  test("Symbol.species exists and is a symbol", () => {
    expect(typeof Symbol.species).toBe("symbol");
  });

  test("Symbol.hasInstance exists and is a symbol", () => {
    expect(typeof Symbol.hasInstance).toBe("symbol");
  });

  test("Symbol.toPrimitive exists and is a symbol", () => {
    expect(typeof Symbol.toPrimitive).toBe("symbol");
  });

  test("Symbol.toStringTag exists and is a symbol", () => {
    expect(typeof Symbol.toStringTag).toBe("symbol");
  });

  test("Symbol.isConcatSpreadable exists and is a symbol", () => {
    expect(typeof Symbol.isConcatSpreadable).toBe("symbol");
  });

  test("Symbol.iterator exists and is a symbol", () => {
    expect(typeof Symbol.iterator).toBe("symbol");
  });

  test("Symbol.match exists and is a symbol", () => {
    expect(typeof Symbol.match).toBe("symbol");
  });

  test("Symbol.matchAll exists and is a symbol", () => {
    expect(typeof Symbol.matchAll).toBe("symbol");
  });

  test("Symbol.replace exists and is a symbol", () => {
    expect(typeof Symbol.replace).toBe("symbol");
  });

  test("Symbol.search exists and is a symbol", () => {
    expect(typeof Symbol.search).toBe("symbol");
  });

  test("Symbol.split exists and is a symbol", () => {
    expect(typeof Symbol.split).toBe("symbol");
  });

  test("well-known symbols are unique", () => {
    expect(Symbol.species !== Symbol.hasInstance).toBe(true);
    expect(Symbol.species !== Symbol.toPrimitive).toBe(true);
    expect(Symbol.species !== Symbol.toStringTag).toBe(true);
    expect(Symbol.species !== Symbol.isConcatSpreadable).toBe(true);
    expect(Symbol.species !== Symbol.iterator).toBe(true);
    expect(Symbol.match !== Symbol.matchAll).toBe(true);
    expect(Symbol.match !== Symbol.replace).toBe(true);
    expect(Symbol.match !== Symbol.search).toBe(true);
    expect(Symbol.match !== Symbol.split).toBe(true);
  });

  test("well-known symbols are stable across accesses", () => {
    expect(Symbol.species).toBe(Symbol.species);
    expect(Symbol.hasInstance).toBe(Symbol.hasInstance);
    expect(Symbol.toPrimitive).toBe(Symbol.toPrimitive);
    expect(Symbol.toStringTag).toBe(Symbol.toStringTag);
    expect(Symbol.isConcatSpreadable).toBe(Symbol.isConcatSpreadable);
    expect(Symbol.match).toBe(Symbol.match);
    expect(Symbol.matchAll).toBe(Symbol.matchAll);
    expect(Symbol.replace).toBe(Symbol.replace);
    expect(Symbol.search).toBe(Symbol.search);
    expect(Symbol.split).toBe(Symbol.split);
  });
});
