/*---
description: Math.E
features: [Math.E]
---*/

describe("Math.E", () => {
  test("has the exact specified approximation", () => {
    expect(Math.E).toBe(2.718281828459045);
  });

  test("is read-only, non-enumerable, and non-configurable", () => {
    const descriptor = Object.getOwnPropertyDescriptor(Math, "E");
    expect(descriptor.writable).toBe(false);
    expect(descriptor.enumerable).toBe(false);
    expect(descriptor.configurable).toBe(false);
    expect(Reflect.set(Math, "E", 1)).toBe(false);
  });
});
