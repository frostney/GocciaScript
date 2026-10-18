/*---
description: Async instance methods on classes
features: [async-await]
---*/

describe("async class method", () => {
  test("returns a Promise and resolves to the return value", () => {
    class MyClass {
      async getValue() {
        return 42;
      }
    }

    const obj = new MyClass();
    const result = obj.getValue();

    expect(result instanceof Promise).toBe(true);
    return result.then((v) => {
      expect(v).toBe(42);
    });
  });

  test("supports await and preserves this", () => {
    class MyClass {
      constructor() {
        this.value = 100;
      }

      async fetch() {
        const a = await Promise.resolve(10);
        const b = await Promise.resolve(20);
        return a + b;
      }

      async getValue() {
        const extra = await Promise.resolve(5);
        return this.value + extra;
      }
    }

    const obj = new MyClass();
    return Promise.all([obj.fetch(), obj.getValue()]).then(([sum, value]) => {
      expect(sum).toBe(30);
      expect(value).toBe(105);
    });
  });

  test("a method named async may put its parameters on the next line", () => {
    // ES2026 §15.8: only an `async` prefix needs the name on its line; this
    // is a plain method named `async`
    class Named {
      async
      () {
        return 4;
      }
    }

    expect(Object.keys(new Named())).toEqual([]);
    expect(new Named().async()).toBe(4);
  });
});
