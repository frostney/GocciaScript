/*---
description: Async methods in object literals
features: [async-await]
---*/

describe("async object methods", () => {
  test("async method in object literal returns Promise", () => {
    const obj = {
      async fetch() {
        return 42;
      }
    };
    expect(obj.fetch() instanceof Promise).toBe(true);
  });

  test("async method in object literal resolves", () => {
    const obj = {
      async compute() {
        const a = await Promise.resolve(3);
        const b = await Promise.resolve(7);
        return a * b;
      }
    };
    return obj.compute().then((v) => {
      expect(v).toBe(21);
    });
  });

  test("async method preserves this in object", () => {
    const obj = {
      value: 50,
      async getTotal() {
        const bonus = await Promise.resolve(10);
        return this.value + bonus;
      }
    };
    return obj.getTotal().then((v) => {
      expect(v).toBe(60);
    });
  });
});
