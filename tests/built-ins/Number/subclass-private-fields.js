describe("Number subclass private fields", () => {
  test("private field on number subclass", () => {
    class LabeledNumber extends Number {
      #label;
      constructor(value, label) {
        super(value);
        this.#label = label;
      }
      getLabel() {
        return this.#label;
      }
    }
    const n = new LabeledNumber(42, "answer");
    expect(n.getLabel()).toBe("answer");
    expect(n.valueOf()).toBe(42);
  });

  test("private field with initializer on number subclass", () => {
    class TrackedNumber extends Number {
      #readCount = 0;
      read() {
        this.#readCount = this.#readCount + 1;
        return this.#readCount;
      }
    }
    const n = new TrackedNumber(100);
    expect(n.read()).toBe(1);
    expect(n.read()).toBe(2);
  });

  test("property initializer on number subclass", () => {
    class NamedNumber extends Number {
      name = "default";
    }
    const n = new NamedNumber(7);
    expect(n.name).toBe("default");
    expect(n.valueOf()).toBe(7);
  });

  test("instanceof checks with private fields", () => {
    class MyNumber extends Number {
      #tag = "num";
      getTag() {
        return this.#tag;
      }
    }
    const n = new MyNumber(3.14);
    expect(n instanceof Number).toBe(true);
    expect(n instanceof MyNumber).toBe(true);
    expect(n.getTag()).toBe("num");
  });
});
