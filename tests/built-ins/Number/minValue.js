/*---
description: Number.MIN_VALUE
features: [Number.MIN_VALUE]
---*/

describe("Number.MIN_VALUE", () => {
  test("has the smallest positive IEEE 754 binary64 value", () => {
    expect(Number.MIN_VALUE).toBe(5e-324);
    expect(Number.MIN_VALUE > 0).toBe(true);
    expect(Number.MIN_VALUE / 2).toBe(0);
  });

  test("is a read-only, non-enumerable, non-configurable property", () => {
    const descriptor = Object.getOwnPropertyDescriptor(Number, "MIN_VALUE");
    expect(descriptor.value).toBe(5e-324);
    expect(descriptor.writable).toBe(false);
    expect(descriptor.enumerable).toBe(false);
    expect(descriptor.configurable).toBe(false);
    expect(Reflect.set(Number, "MIN_VALUE", 1)).toBe(false);
    expect(Number.MIN_VALUE).toBe(5e-324);
  });
});
