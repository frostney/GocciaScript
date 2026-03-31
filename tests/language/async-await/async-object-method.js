/*---
description: Async methods in object literals
features: [async-await]
---*/

describe("async object method", () => {
  test("returns a Promise", () => {
    const obj = {
      async fetch() {
        return 42;
      }
    };
    expect(obj.fetch() instanceof Promise).toBe(true);
  });

  test("supports await and preserves this", () => {
    const obj = {
      value: 50,
      async compute() {
        const a = await Promise.resolve(3);
        const b = await Promise.resolve(7);
        return a * b;
      },
      async getTotal() {
        const bonus = await Promise.resolve(10);
        return this.value + bonus;
      }
    };

    return Promise.all([obj.compute(), obj.getTotal()]).then(([computed, total]) => {
      expect(computed).toBe(21);
      expect(total).toBe(60);
    });
  });
});
