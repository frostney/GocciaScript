/*---
description: Number.MAX_SAFE_INTEGER
features: [Number.MAX_SAFE_INTEGER]
---*/

describe("Number.MAX_SAFE_INTEGER", () => {
  test("has the largest exactly representable integer with unique successor", () => {
    expect(Number.MAX_SAFE_INTEGER).toBe(9007199254740991);
    expect(Number.isSafeInteger(Number.MAX_SAFE_INTEGER)).toBe(true);
    expect(Number.isSafeInteger(Number.MAX_SAFE_INTEGER + 1)).toBe(false);
  });

  test("is a read-only, non-enumerable, non-configurable property", () => {
    const descriptor = Object.getOwnPropertyDescriptor(Number, "MAX_SAFE_INTEGER");
    expect(descriptor.value).toBe(9007199254740991);
    expect(descriptor.writable).toBe(false);
    expect(descriptor.enumerable).toBe(false);
    expect(descriptor.configurable).toBe(false);
    expect(Reflect.set(Number, "MAX_SAFE_INTEGER", 1)).toBe(false);
    expect(Number.MAX_SAFE_INTEGER).toBe(9007199254740991);
  });
});
