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
});
