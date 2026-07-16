/*---
description: Math.SQRT1_2
features: [Math.SQRT1_2]
---*/

describe("Math.SQRT1_2", () => {
  test("has the exact specified approximation", () => {
    expect(Math.SQRT1_2).toBe(0.7071067811865476);
  });

  test("is read-only, non-enumerable, and non-configurable", () => {
    const descriptor = Object.getOwnPropertyDescriptor(Math, "SQRT1_2");
    expect(descriptor.writable).toBe(false);
    expect(descriptor.enumerable).toBe(false);
    expect(descriptor.configurable).toBe(false);
    expect(Reflect.set(Math, "SQRT1_2", 1)).toBe(false);
  });
});
