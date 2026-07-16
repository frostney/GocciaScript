/*---
description: Number.MAX_VALUE
features: [Number.MAX_VALUE]
---*/

describe("Number.MAX_VALUE", () => {
  test("has the largest finite IEEE 754 binary64 value", () => {
    expect(Number.MAX_VALUE).toBe(1.7976931348623157e308);
    expect(Number.isFinite(Number.MAX_VALUE)).toBe(true);
    expect(Number.MAX_VALUE * 2).toBe(Infinity);
  });

  test("is a read-only, non-enumerable, non-configurable property", () => {
    const descriptor = Object.getOwnPropertyDescriptor(Number, "MAX_VALUE");
    expect(descriptor.value).toBe(1.7976931348623157e308);
    expect(descriptor.writable).toBe(false);
    expect(descriptor.enumerable).toBe(false);
    expect(descriptor.configurable).toBe(false);
    expect(Reflect.set(Number, "MAX_VALUE", 1)).toBe(false);
    expect(Number.MAX_VALUE).toBe(1.7976931348623157e308);
  });
});
