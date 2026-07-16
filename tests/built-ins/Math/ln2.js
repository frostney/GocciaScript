/*---
description: Math.LN2
features: [Math.LN2]
---*/

describe("Math.LN2", () => {
  test("has the exact specified approximation", () => {
    expect(Math.LN2).toBe(0.6931471805599453);
  });

  test("is read-only, non-enumerable, and non-configurable", () => {
    const descriptor = Object.getOwnPropertyDescriptor(Math, "LN2");
    expect(descriptor.writable).toBe(false);
    expect(descriptor.enumerable).toBe(false);
    expect(descriptor.configurable).toBe(false);
    expect(Reflect.set(Math, "LN2", 1)).toBe(false);
  });
});
