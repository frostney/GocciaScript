describe("Symbol.species", () => {
  test("Symbol.species is a symbol", () => {
    expect(typeof Symbol.species).toBe("symbol");
  });

  test("Array[Symbol.species] returns Array constructor", () => {
    expect(Array[Symbol.species]).toBe(Array);
  });

  test("Map[Symbol.species] returns Map constructor", () => {
    expect(Map[Symbol.species]).toBe(Map);
  });

  test("Set[Symbol.species] returns Set constructor", () => {
    expect(Set[Symbol.species]).toBe(Set);
  });

  test("Promise[Symbol.species] returns Promise constructor", () => {
    expect(Promise[Symbol.species]).toBe(Promise);
  });

  test("RegExp[Symbol.species] returns RegExp constructor", () => {
    expect(RegExp[Symbol.species]).toBe(RegExp);
  });

  test("Symbol.species accessor names", () => {
    expect(Object.getOwnPropertyDescriptor(Array, Symbol.species).get.name).toBe("get [Symbol.species]");
    expect(Object.getOwnPropertyDescriptor(Map, Symbol.species).get.name).toBe("get [Symbol.species]");
    expect(Object.getOwnPropertyDescriptor(Promise, Symbol.species).get.name).toBe("get [Symbol.species]");
    expect(Object.getOwnPropertyDescriptor(RegExp, Symbol.species).get.name).toBe("get [Symbol.species]");
    expect(Object.getOwnPropertyDescriptor(Set, Symbol.species).get.name).toBe("get [Symbol.species]");
  });

  test("RegExp subclass inherits Symbol.species through constructor prototype chain", () => {
    class MyRegExp extends RegExp {}
    expect(MyRegExp[Symbol.species]).toBe(MyRegExp);
  });
});
