/*---
description: Math.LOG2E
features: [Math.LOG2E]
---*/

describe("Math.LOG2E", () => {
  test("has the exact specified approximation", () => {
    expect(Math.LOG2E).toBe(1.4426950408889634);
  });

  test("is read-only, non-enumerable, and non-configurable", () => {
    const descriptor = Object.getOwnPropertyDescriptor(Math, "LOG2E");
    expect(descriptor.writable).toBe(false);
    expect(descriptor.enumerable).toBe(false);
    expect(descriptor.configurable).toBe(false);
    expect(Reflect.set(Math, "LOG2E", 1)).toBe(false);
  });
});
