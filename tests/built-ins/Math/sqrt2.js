/*---
description: Math.SQRT2
features: [Math.SQRT2]
---*/

describe("Math.SQRT2", () => {
  test("has the exact specified approximation", () => {
    expect(Math.SQRT2).toBe(1.4142135623730951);
  });

  test("is read-only, non-enumerable, and non-configurable", () => {
    const descriptor = Object.getOwnPropertyDescriptor(Math, "SQRT2");
    expect(descriptor.writable).toBe(false);
    expect(descriptor.enumerable).toBe(false);
    expect(descriptor.configurable).toBe(false);
    expect(Reflect.set(Math, "SQRT2", 1)).toBe(false);
  });
});
