describe("Boolean subclass private fields", () => {
  test("private field on boolean subclass", () => {
    class ReasonedBoolean extends Boolean {
      #reason;
      constructor(value, reason) {
        super(value);
        this.#reason = reason;
      }
      getReason() {
        return this.#reason;
      }
    }
    const b = new ReasonedBoolean(true, "user approved");
    expect(b.getReason()).toBe("user approved");
    expect(b.valueOf()).toBe(true);
  });

  test("private field with initializer on boolean subclass", () => {
    class ToggleBoolean extends Boolean {
      #toggleCount = 0;
      toggle() {
        this.#toggleCount = this.#toggleCount + 1;
        return this.#toggleCount;
      }
    }
    const b = new ToggleBoolean(false);
    expect(b.toggle()).toBe(1);
    expect(b.toggle()).toBe(2);
  });

  test("property initializer on boolean subclass", () => {
    class LabeledBoolean extends Boolean {
      label = "flag";
    }
    const b = new LabeledBoolean(true);
    expect(b.label).toBe("flag");
    expect(b.valueOf()).toBe(true);
  });

  test("instanceof checks with private fields", () => {
    class MyBoolean extends Boolean {
      #id = 33;
      getId() {
        return this.#id;
      }
    }
    const b = new MyBoolean(false);
    expect(b instanceof Boolean).toBe(true);
    expect(b instanceof MyBoolean).toBe(true);
    expect(b.getId()).toBe(33);
  });
});
