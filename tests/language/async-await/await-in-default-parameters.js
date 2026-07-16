/*---
description: Async functions evaluate ordinary parameter defaults before the body
features: [async-await]
---*/

describe("await in async function parameter defaults", () => {
  test("async arrow with default parameter works normally", async () => {
    const fn = async (x = 42) => x;
    expect(await fn()).toBe(42);
    expect(await fn(10)).toBe(10);
  });

  test("async arrow body can still use await", async () => {
    const fn = async (x = 42) => {
      return await Promise.resolve(x);
    };
    expect(await fn()).toBe(42);
  });
});
