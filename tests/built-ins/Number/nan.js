/*---
description: Number.NaN
features: [Number.NaN]
---*/

describe("Number.NaN", () => {
  test("is the NaN number value", () => {
    expect(Number.isNaN(Number.NaN)).toBe(true);
    expect(Object.is(Number.NaN, NaN)).toBe(true);
  });

  test("is a read-only, non-enumerable, non-configurable property", () => {
    const descriptor = Object.getOwnPropertyDescriptor(Number, "NaN");
    expect(Number.isNaN(descriptor.value)).toBe(true);
    expect(descriptor.writable).toBe(false);
    expect(descriptor.enumerable).toBe(false);
    expect(descriptor.configurable).toBe(false);
    expect(Reflect.set(Number, "NaN", 1)).toBe(false);
    expect(Number.isNaN(Number.NaN)).toBe(true);
  });
});
