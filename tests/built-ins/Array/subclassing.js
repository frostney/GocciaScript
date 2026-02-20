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

  test("map returns subclass instance via Symbol.species", () => {
    class MyArray extends Array {}
    const arr = new MyArray(1, 2, 3);
    const mapped = arr.map((x) => x * 2);
    expect(mapped instanceof MyArray).toBe(true);
    expect(mapped instanceof Array).toBe(true);
  });

  test("filter returns subclass instance via Symbol.species", () => {
    class MyArray extends Array {}
    const arr = new MyArray(1, 2, 3, 4);
    const filtered = arr.filter((x) => x > 2);
    expect(filtered instanceof MyArray).toBe(true);
    expect(filtered[0]).toBe(3);
    expect(filtered[1]).toBe(4);
  });

  test("slice returns subclass instance via Symbol.species", () => {
    class MyArray extends Array {}
    const arr = new MyArray(1, 2, 3, 4);
    const sliced = arr.slice(1, 3);
    expect(sliced instanceof MyArray).toBe(true);
    expect(sliced[0]).toBe(2);
    expect(sliced[1]).toBe(3);
  });

  test("concat returns subclass instance via Symbol.species", () => {
    class MyArray extends Array {}
    const arr = new MyArray(1, 2);
    const result = arr.concat([3, 4]);
    expect(result instanceof MyArray).toBe(true);
    expect(result.length).toBe(4);
  });

  test("flat returns subclass instance via Symbol.species", () => {
    class MyArray extends Array {}
    const arr = new MyArray([1, 2], [3, 4]);
    const result = arr.flat();
    expect(result instanceof MyArray).toBe(true);
  });

  test("flatMap returns subclass instance via Symbol.species", () => {
    class MyArray extends Array {}
    const arr = new MyArray(1, 2);
    const result = arr.flatMap((x) => [x, x * 2]);
    expect(result instanceof MyArray).toBe(true);
  });

  test("splice returns subclass instance via Symbol.species", () => {
    class MyArray extends Array {}
    const arr = new MyArray(1, 2, 3, 4);
    const removed = arr.splice(1, 2);
    expect(removed instanceof MyArray).toBe(true);
    expect(removed.length).toBe(2);
  });

  test("overriding Symbol.species to Array returns plain Array", () => {
    class MyArray extends Array {
      static get [Symbol.species]() {
        return Array;
      }
    }
    const arr = new MyArray(1, 2, 3);
    const mapped = arr.map((x) => x * 2);
    expect(mapped instanceof MyArray).toBe(false);
    expect(mapped instanceof Array).toBe(true);
    expect(mapped[0]).toBe(2);
    expect(mapped[1]).toBe(4);
    expect(mapped[2]).toBe(6);
  });

  test("Symbol.species returning null falls back to plain Array", () => {
    class MyArray extends Array {
      static get [Symbol.species]() {
        return null;
      }
    }
    const arr = new MyArray(1, 2);
    const mapped = arr.map((x) => x);
    expect(mapped instanceof Array).toBe(true);
  });

  test("Symbol.species returning undefined falls back to plain Array", () => {
    class MyArray extends Array {
      static get [Symbol.species]() {
        return undefined;
      }
    }
    const arr = new MyArray(1, 2);
    const mapped = arr.map((x) => x);
    expect(mapped instanceof Array).toBe(true);
  });

  test("subclass inherits Symbol.species from Array", () => {
    class MyArray extends Array {}
    expect(MyArray[Symbol.species]).toBe(MyArray);
  });

  test("sub-subclass inherits Symbol.species", () => {
    class MyArray extends Array {}
    class MySuperArray extends MyArray {}
    expect(MySuperArray[Symbol.species]).toBe(MySuperArray);
  });

  test("overridden species is used by sub-subclass, not inherited", () => {
    class MyArray extends Array {
      static get [Symbol.species]() {
        return Array;
      }
    }
    class SubMyArray extends MyArray {}
    expect(SubMyArray[Symbol.species]).toBe(Array);
  });
});
