/*---
description: Number.NEGATIVE_INFINITY
features: [Number.NEGATIVE_INFINITY]
---*/

describe("Number.NEGATIVE_INFINITY", () => {
  test("is negative infinity", () => {
    expect(Number.NEGATIVE_INFINITY).toBe(-Infinity);
    expect(Number.isFinite(Number.NEGATIVE_INFINITY)).toBe(false);
  });

  test("is a read-only, non-enumerable, non-configurable property", () => {
    const descriptor = Object.getOwnPropertyDescriptor(Number, "NEGATIVE_INFINITY");
    expect(descriptor.value).toBe(-Infinity);
    expect(descriptor.writable).toBe(false);
    expect(descriptor.enumerable).toBe(false);
    expect(descriptor.configurable).toBe(false);
    expect(Reflect.set(Number, "NEGATIVE_INFINITY", 1)).toBe(false);
    expect(Number.NEGATIVE_INFINITY).toBe(-Infinity);
  });
});
