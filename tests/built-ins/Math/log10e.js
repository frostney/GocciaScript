/*---
description: Math.LOG10E
features: [Math.LOG10E]
---*/

describe("Math.LOG10E", () => {
  test("has the exact specified approximation", () => {
    expect(Math.LOG10E).toBe(0.4342944819032518);
  });

  test("is read-only, non-enumerable, and non-configurable", () => {
    const descriptor = Object.getOwnPropertyDescriptor(Math, "LOG10E");
    expect(descriptor.writable).toBe(false);
    expect(descriptor.enumerable).toBe(false);
    expect(descriptor.configurable).toBe(false);
    expect(Reflect.set(Math, "LOG10E", 1)).toBe(false);
  });
});
