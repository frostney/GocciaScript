/*---
description: Async static methods on classes
features: [async-await]
---*/

test("async static methods return Promises", () => {
  class MyClass {
    static async create() {
      const val = await Promise.resolve("created");
      return val;
    }
  }

  const result = MyClass.create();
  expect(result instanceof Promise).toBe(true);

  return result.then((v) => {
    expect(v).toBe("created");
  });
});
