/*---
description: Set.prototype.constructor
features: [Set]
---*/

test("links Set.prototype and Set instances back to Set", () => {
  expect(Set.prototype.constructor).toBe(Set);
  expect(new Set().constructor).toBe(Set);
});

test("has the standard property descriptor", () => {
  const descriptor = Object.getOwnPropertyDescriptor(Set.prototype, "constructor");
  expect(descriptor.writable).toBe(true);
  expect(descriptor.enumerable).toBe(false);
  expect(descriptor.configurable).toBe(true);
});
