describe("Set subclass private fields", () => {
  test("private field on set subclass", () => {
    class LabeledSet extends Set {
      #label;
      constructor(label, values) {
        super(values);
        this.#label = label;
      }
      getLabel() {
        return this.#label;
      }
    }
    const s = new LabeledSet("tags", [1, 2, 3]);
    expect(s.getLabel()).toBe("tags");
    expect(s.has(1)).toBe(true);
    expect(s.size).toBe(3);
  });

  test("private field with initializer on set subclass", () => {
    class CountingSet extends Set {
      #addCount = 0;
      trackedAdd(value) {
        this.#addCount = this.#addCount + 1;
        this.add(value);
        return this.#addCount;
      }
    }
    const s = new CountingSet();
    expect(s.trackedAdd("a")).toBe(1);
    expect(s.trackedAdd("b")).toBe(2);
    expect(s.size).toBe(2);
  });

  test("property initializer on set subclass", () => {
    class NamedSet extends Set {
      name = "default-set";
    }
    const s = new NamedSet();
    expect(s.name).toBe("default-set");
    expect(s.size).toBe(0);
  });

  test("instanceof checks with private fields", () => {
    class MySet extends Set {
      #id = 77;
      getId() {
        return this.#id;
      }
    }
    const s = new MySet([10, 20]);
    expect(s instanceof Set).toBe(true);
    expect(s instanceof MySet).toBe(true);
    expect(s.getId()).toBe(77);
    expect(s.has(10)).toBe(true);
  });
});
