describe("Array subclass private fields", () => {
  test("private field on array subclass", () => {
    class TaggedArray extends Array {
      #tag;
      constructor(tag, ...items) {
        super(...items);
        this.#tag = tag;
      }
      getTag() {
        return this.#tag;
      }
    }
    const arr = new TaggedArray("numbers", 1, 2, 3);
    expect(arr.getTag()).toBe("numbers");
    expect(arr.length).toBe(3);
    expect(arr[0]).toBe(1);
  });

  test("private field with initializer on array subclass", () => {
    class CountingArray extends Array {
      #accessCount = 0;
      trackAccess() {
        this.#accessCount = this.#accessCount + 1;
        return this.#accessCount;
      }
    }
    const arr = new CountingArray(10, 20, 30);
    expect(arr.trackAccess()).toBe(1);
    expect(arr.trackAccess()).toBe(2);
    expect(arr.length).toBe(3);
  });

  test("property initializer on array subclass", () => {
    class LabeledArray extends Array {
      label = "default";
    }
    const arr = new LabeledArray(1, 2);
    expect(arr.label).toBe("default");
    expect(arr.length).toBe(2);
  });

  test("multiple private fields on array subclass", () => {
    class MetadataArray extends Array {
      #createdAt;
      #name;
      constructor(name, createdAt, ...items) {
        super(...items);
        this.#name = name;
        this.#createdAt = createdAt;
      }
      getName() {
        return this.#name;
      }
      getCreatedAt() {
        return this.#createdAt;
      }
    }
    const arr = new MetadataArray("test", 12345, 1, 2);
    expect(arr.getName()).toBe("test");
    expect(arr.getCreatedAt()).toBe(12345);
    expect(arr.length).toBe(2);
  });

  test("private method on array subclass", () => {
    class ValidatedArray extends Array {
      #validate(item) {
        return item > 0;
      }
      addIfValid(item) {
        if (this.#validate(item)) {
          this.push(item);
          return true;
        }
        return false;
      }
    }
    const arr = new ValidatedArray();
    expect(arr.addIfValid(5)).toBe(true);
    expect(arr.addIfValid(-1)).toBe(false);
    expect(arr.length).toBe(1);
  });

  test("instanceof checks with private fields", () => {
    class MyArray extends Array {
      #id = 42;
      getId() {
        return this.#id;
      }
    }
    const arr = new MyArray(1, 2, 3);
    expect(arr instanceof Array).toBe(true);
    expect(arr instanceof MyArray).toBe(true);
    expect(arr.getId()).toBe(42);
  });
});
