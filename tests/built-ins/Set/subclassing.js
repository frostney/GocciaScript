describe("Set subclassing", () => {
  test("new Set() creates empty set", () => {
    const s = new Set();
    expect(s.size).toBe(0);
  });

  test("new Set(iterable) populates from values", () => {
    const s = new Set([1, 2, 3]);
    expect(s.size).toBe(3);
    expect(s.has(1)).toBe(true);
    expect(s.has(2)).toBe(true);
    expect(s.has(3)).toBe(true);
  });

  test("Set instanceof Set", () => {
    const s = new Set();
    expect(s instanceof Set).toBe(true);
  });

  test("class extends Set creates set instance", () => {
    class MySet extends Set {}
    const s = new MySet([1, 2, 3]);
    expect(s.size).toBe(3);
    expect(s.has(1)).toBe(true);
  });

  test("subclass instanceof both Set and subclass", () => {
    class MySet extends Set {}
    const s = new MySet();
    expect(s instanceof Set).toBe(true);
    expect(s instanceof MySet).toBe(true);
  });

  test("subclass has set methods", () => {
    class MySet extends Set {}
    const s = new MySet();
    s.add(42);
    expect(s.has(42)).toBe(true);
    expect(s.size).toBe(1);
  });

  test("subclass inherits Symbol.species from Set", () => {
    class MySet extends Set {}
    expect(MySet[Symbol.species]).toBe(MySet);
  });
});
