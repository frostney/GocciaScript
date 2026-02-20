describe("Map subclassing", () => {
  test("new Map() creates empty map", () => {
    const m = new Map();
    expect(m.size).toBe(0);
  });

  test("new Map(iterable) populates from entries", () => {
    const m = new Map([["a", 1], ["b", 2]]);
    expect(m.size).toBe(2);
    expect(m.get("a")).toBe(1);
    expect(m.get("b")).toBe(2);
  });

  test("Map instanceof Map", () => {
    const m = new Map();
    expect(m instanceof Map).toBe(true);
  });

  test("class extends Map creates map instance", () => {
    class MyMap extends Map {}
    const m = new MyMap([["a", 1]]);
    expect(m.size).toBe(1);
    expect(m.get("a")).toBe(1);
  });

  test("subclass instanceof both Map and subclass", () => {
    class MyMap extends Map {}
    const m = new MyMap();
    expect(m instanceof Map).toBe(true);
    expect(m instanceof MyMap).toBe(true);
  });

  test("subclass has map methods", () => {
    class MyMap extends Map {}
    const m = new MyMap();
    m.set("x", 42);
    expect(m.has("x")).toBe(true);
    expect(m.get("x")).toBe(42);
  });
});
