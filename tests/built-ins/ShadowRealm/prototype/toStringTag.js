/*---
description: ShadowRealm.prototype Symbol.toStringTag
features: [ShadowRealm, Symbol.toStringTag]
---*/

test("is ShadowRealm with the standard descriptor", () => {
  expect(ShadowRealm.prototype[Symbol.toStringTag]).toBe("ShadowRealm");
  const descriptor = Object.getOwnPropertyDescriptor(
    ShadowRealm.prototype,
    Symbol.toStringTag,
  );
  expect(descriptor.writable).toBe(false);
  expect(descriptor.enumerable).toBe(false);
  expect(descriptor.configurable).toBe(true);
});
