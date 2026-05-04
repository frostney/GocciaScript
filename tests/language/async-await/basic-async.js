/*---
description: Basic async function declarations return Promises
features: [async-await]
---*/

describe("async functions", () => {
  test("async arrow function returns a Promise", () => {
    const fn = async () => 42;
    const result = fn();
    expect(result instanceof Promise).toBe(true);
  });

  test("async arrow function resolves with return value", () => {
    const fn = async () => 42;
    return fn().then((v) => {
      expect(v).toBe(42);
    });
  });

  test("async arrow function with no return resolves to undefined", () => {
    const fn = async () => {};
    return fn().then((v) => {
      expect(v).toBeUndefined();
    });
  });

  test("async function with explicit return", () => {
    const fn = async () => {
      return "hello";
    };
    return fn().then((v) => {
      expect(v).toBe("hello");
    });
  });

  test("async function returning a Promise resolves the inner Promise", () => {
    const fn = async () => {
      return Promise.resolve(99);
    };
    return fn().then((v) => {
      expect(v).toBe(99);
    });
  });

  test("async function returning a rejected Promise propagates rejection", async () => {
    const fn = async () => Promise.reject("async rejection");

    try {
      await fn();
    } catch (e) {
      expect(e).toBe("async rejection");
      return;
    }

    expect(true).toBe(false);
  });

  test("await accepts private field shorthand operands", async () => {
    class Counter {
      #value = Promise.resolve(42);

      async read() {
        return await #value;
      }
    }

    expect(await new Counter().read()).toBe(42);
  });
});
