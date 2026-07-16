/*---
description: Math.PI
features: [Math.PI]
---*/

describe("Math.PI", () => {
  test("has the exact specified approximation", () => {
    expect(Math.PI).toBe(3.141592653589793);
  });

  test("is read-only, non-enumerable, and non-configurable", () => {
    const descriptor = Object.getOwnPropertyDescriptor(Math, "PI");
    expect(descriptor.writable).toBe(false);
    expect(descriptor.enumerable).toBe(false);
    expect(descriptor.configurable).toBe(false);
    expect(Reflect.set(Math, "PI", 1)).toBe(false);
  });
});
