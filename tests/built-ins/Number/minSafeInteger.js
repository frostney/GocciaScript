/*---
description: Number.MIN_SAFE_INTEGER
features: [Number.MIN_SAFE_INTEGER]
---*/

describe("Number.MIN_SAFE_INTEGER", () => {
  test("has the smallest exactly representable integer with unique predecessor", () => {
    expect(Number.MIN_SAFE_INTEGER).toBe(-9007199254740991);
    expect(Number.isSafeInteger(Number.MIN_SAFE_INTEGER)).toBe(true);
    expect(Number.isSafeInteger(Number.MIN_SAFE_INTEGER - 1)).toBe(false);
  });

  test("is a read-only, non-enumerable, non-configurable property", () => {
    const descriptor = Object.getOwnPropertyDescriptor(Number, "MIN_SAFE_INTEGER");
    expect(descriptor.value).toBe(-9007199254740991);
    expect(descriptor.writable).toBe(false);
    expect(descriptor.enumerable).toBe(false);
    expect(descriptor.configurable).toBe(false);
    expect(Reflect.set(Number, "MIN_SAFE_INTEGER", 1)).toBe(false);
    expect(Number.MIN_SAFE_INTEGER).toBe(-9007199254740991);
  });
});
