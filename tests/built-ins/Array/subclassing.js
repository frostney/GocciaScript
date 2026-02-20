describe("Array subclassing", () => {
  test("class extends Array creates array instance", () => {
    class MyArray extends Array {}
    const arr = new MyArray(1, 2, 3);
    expect(arr.length).toBe(3);
    expect(arr[0]).toBe(1);
    expect(arr[1]).toBe(2);
    expect(arr[2]).toBe(3);
  });

  test("subclass instanceof both Array and subclass", () => {
    class MyArray extends Array {}
    const arr = new MyArray();
    expect(arr instanceof Array).toBe(true);
    expect(arr instanceof MyArray).toBe(true);
  });

  test("subclass has array methods", () => {
    class MyArray extends Array {}
    const arr = new MyArray(1, 2, 3);
    expect(arr.map((x) => x * 2).join(",")).toBe("2,4,6");
  });

  test("subclass with custom method", () => {
    class MyArray extends Array {
      sum() {
        return this.reduce((a, b) => a + b, 0);
      }
    }
    const arr = new MyArray(1, 2, 3);
    expect(arr.sum()).toBe(6);
  });

  test("Array.from respects this as constructor", () => {
    class MyArray extends Array {}
    const arr = MyArray.from([1, 2, 3]);
    expect(arr instanceof MyArray).toBe(true);
    expect(arr.length).toBe(3);
  });

  test("Array.of respects this as constructor", () => {
    class MyArray extends Array {}
    const arr = MyArray.of(1, 2, 3);
    expect(arr instanceof MyArray).toBe(true);
    expect(arr.length).toBe(3);
  });

  test("subclass constructor with super(...args) spread", () => {
    class MyArray extends Array {
      constructor(...args) {
        super(...args);
      }
    }
    const arr = new MyArray(1, 2, 3);
    expect(arr.length).toBe(3);
    expect(arr[0]).toBe(1);
    expect(arr[1]).toBe(2);
    expect(arr[2]).toBe(3);
    expect(arr instanceof Array).toBe(true);
    expect(arr instanceof MyArray).toBe(true);
  });

  test("subclass constructor with super() and custom logic", () => {
    class TaggedArray extends Array {
      constructor(tag, ...items) {
        super(...items);
        this.tag = tag;
      }
    }
    const arr = new TaggedArray("nums", 10, 20, 30);
    expect(arr.tag).toBe("nums");
    expect(arr.length).toBe(3);
    expect(arr[0]).toBe(10);
    expect(arr[1]).toBe(20);
    expect(arr[2]).toBe(30);
  });
});
