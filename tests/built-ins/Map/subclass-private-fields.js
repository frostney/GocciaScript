describe("Map subclass private fields", () => {
  test("private field on map subclass", () => {
    class LabeledMap extends Map {
      #label;
      constructor(label, entries) {
        super(entries);
        this.#label = label;
      }
      getLabel() {
        return this.#label;
      }
    }
    const m = new LabeledMap("config", [["key", "value"]]);
    expect(m.getLabel()).toBe("config");
    expect(m.get("key")).toBe("value");
    expect(m.size).toBe(1);
  });

  test("private field with initializer on map subclass", () => {
    class TrackedMap extends Map {
      #setCount = 0;
      trackedSet(key, value) {
        this.#setCount = this.#setCount + 1;
        this.set(key, value);
        return this.#setCount;
      }
    }
    const m = new TrackedMap();
    expect(m.trackedSet("a", 1)).toBe(1);
    expect(m.trackedSet("b", 2)).toBe(2);
    expect(m.size).toBe(2);
  });

  test("property initializer on map subclass", () => {
    class NamedMap extends Map {
      name = "unnamed";
    }
    const m = new NamedMap();
    expect(m.name).toBe("unnamed");
    expect(m.size).toBe(0);
  });

  test("instanceof checks with private fields", () => {
    class MyMap extends Map {
      #id = 99;
      getId() {
        return this.#id;
      }
    }
    const m = new MyMap([["x", 1]]);
    expect(m instanceof Map).toBe(true);
    expect(m instanceof MyMap).toBe(true);
    expect(m.getId()).toBe(99);
    expect(m.get("x")).toBe(1);
  });
});
