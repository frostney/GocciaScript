describe("String subclass private fields", () => {
  test("private field on string subclass", () => {
    class TaggedString extends String {
      #tag;
      constructor(value, tag) {
        super(value);
        this.#tag = tag;
      }
      getTag() {
        return this.#tag;
      }
    }
    const s = new TaggedString("hello", "greeting");
    expect(s.getTag()).toBe("greeting");
    expect(s.valueOf()).toBe("hello");
  });

  test("private field with initializer on string subclass", () => {
    class AnnotatedString extends String {
      #annotation = "none";
      setAnnotation(value) {
        this.#annotation = value;
      }
      getAnnotation() {
        return this.#annotation;
      }
    }
    const s = new AnnotatedString("text");
    expect(s.getAnnotation()).toBe("none");
    s.setAnnotation("important");
    expect(s.getAnnotation()).toBe("important");
  });

  test("property initializer on string subclass", () => {
    class LabeledString extends String {
      label = "default";
    }
    const s = new LabeledString("test");
    expect(s.label).toBe("default");
    expect(s.valueOf()).toBe("test");
  });

  test("instanceof checks with private fields", () => {
    class MyString extends String {
      #id = 55;
      getId() {
        return this.#id;
      }
    }
    const s = new MyString("world");
    expect(s instanceof String).toBe(true);
    expect(s instanceof MyString).toBe(true);
    expect(s.getId()).toBe(55);
  });
});
