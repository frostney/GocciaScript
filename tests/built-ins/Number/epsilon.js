/*---
description: Number.EPSILON
features: [Number.EPSILON]
---*/

describe("Number.EPSILON", () => {
  test("has the exact difference between one and the next representable number", () => {
    expect(Number.EPSILON).toBe(2.220446049250313e-16);
    expect(1 + Number.EPSILON).not.toBe(1);
    expect(1 + Number.EPSILON / 2).toBe(1);
  });

  test("is a read-only, non-enumerable, non-configurable property", () => {
    const descriptor = Object.getOwnPropertyDescriptor(Number, "EPSILON");
    expect(descriptor.value).toBe(2.220446049250313e-16);
    expect(descriptor.writable).toBe(false);
    expect(descriptor.enumerable).toBe(false);
    expect(descriptor.configurable).toBe(false);
    expect(Reflect.set(Number, "EPSILON", 1)).toBe(false);
    expect(Number.EPSILON).toBe(2.220446049250313e-16);
  });
});
