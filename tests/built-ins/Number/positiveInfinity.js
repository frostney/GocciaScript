/*---
description: Number.POSITIVE_INFINITY
features: [Number.POSITIVE_INFINITY]
---*/

describe("Number.POSITIVE_INFINITY", () => {
  test("is positive infinity", () => {
    expect(Number.POSITIVE_INFINITY).toBe(Infinity);
    expect(Number.isFinite(Number.POSITIVE_INFINITY)).toBe(false);
  });

  test("is a read-only, non-enumerable, non-configurable property", () => {
    const descriptor = Object.getOwnPropertyDescriptor(Number, "POSITIVE_INFINITY");
    expect(descriptor.value).toBe(Infinity);
    expect(descriptor.writable).toBe(false);
    expect(descriptor.enumerable).toBe(false);
    expect(descriptor.configurable).toBe(false);
    expect(Reflect.set(Number, "POSITIVE_INFINITY", 1)).toBe(false);
    expect(Number.POSITIVE_INFINITY).toBe(Infinity);
  });
});
