/*---
description: Math.LN10
features: [Math.LN10]
---*/

describe("Math.LN10", () => {
  test("has the exact specified approximation", () => {
    expect(Math.LN10).toBe(2.302585092994046);
  });

  test("is read-only, non-enumerable, and non-configurable", () => {
    const descriptor = Object.getOwnPropertyDescriptor(Math, "LN10");
    expect(descriptor.writable).toBe(false);
    expect(descriptor.enumerable).toBe(false);
    expect(descriptor.configurable).toBe(false);
    expect(Reflect.set(Math, "LN10", 1)).toBe(false);
  });
});
