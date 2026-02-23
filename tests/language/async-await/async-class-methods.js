/*---
description: Async class methods return Promises and support await
features: [async-await]
---*/

describe("async class methods", () => {
  test("async method returns a Promise", () => {
    class MyClass {
      async getValue() {
        return 42;
      }
    }
    const obj = new MyClass();
    const result = obj.getValue();
    expect(result instanceof Promise).toBe(true);
  });

  test("async method resolves with return value", () => {
    class MyClass {
      async getValue() {
        return 42;
      }
    }
    const obj = new MyClass();
    return obj.getValue().then((v) => {
      expect(v).toBe(42);
    });
  });

  test("async method can use await", () => {
    class MyClass {
      async fetch() {
        const a = await Promise.resolve(10);
        const b = await Promise.resolve(20);
        return a + b;
      }
    }
    const obj = new MyClass();
    return obj.fetch().then((v) => {
      expect(v).toBe(30);
    });
  });

  test("async method preserves this binding", () => {
    class MyClass {
      constructor() {
        this.value = 100;
      }
      async getValue() {
        const extra = await Promise.resolve(5);
        return this.value + extra;
      }
    }
    const obj = new MyClass();
    return obj.getValue().then((v) => {
      expect(v).toBe(105);
    });
  });

  test("async static method", () => {
    class MyClass {
      static async create() {
        const val = await Promise.resolve("created");
        return val;
      }
    }
    return MyClass.create().then((v) => {
      expect(v).toBe("created");
    });
  });
});
