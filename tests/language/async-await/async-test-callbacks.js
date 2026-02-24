/*---
description: Async test functions support await in body and expect calls
features: [async-await]
---*/

describe("async test functions", () => {
  test("async test can use await directly", async () => {
    const result = await Promise.resolve(42);
    expect(result).toBe(42);
  });

  test("async test with multiple awaits", async () => {
    const a = await Promise.resolve(1);
    const b = await Promise.resolve(2);
    expect(a + b).toBe(3);
  });

  test("await inside expect call", async () => {
    expect(await Promise.resolve(42)).toBe(42);
  });

  test("await async function result inside expect", async () => {
    const fetchValue = async () => {
      const raw = await Promise.resolve(10);
      return raw * 3;
    };
    expect(await fetchValue()).toBe(30);
  });

  test("multiple assertions after awaits", async () => {
    const a = await Promise.resolve("hello");
    const b = await Promise.resolve("world");
    expect(a).toBe("hello");
    expect(b).toBe("world");
    expect(a + " " + b).toBe("hello world");
  });

  test("await in expect with toEqual", async () => {
    const getData = async () => {
      const items = await Promise.resolve([1, 2, 3]);
      return items;
    };
    expect(await getData()).toEqual([1, 2, 3]);
  });

  test("await chained Promises in expect", async () => {
    expect(await Promise.resolve(5).then((v) => v * 4)).toBe(20);
  });

  test("async test with try/catch around await", async () => {
    try {
      await Promise.reject("expected failure");
    } catch (e) {
      expect(e).toBe("expected failure");
    }
  });

  test("returning a Promise still works as primary pattern", () => {
    return Promise.resolve(99).then((v) => {
      expect(v).toBe(99);
    });
  });
});
