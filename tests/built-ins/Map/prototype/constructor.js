/*---
description: Map.prototype.constructor
features: [Map]
---*/

test("links Map.prototype and Map instances back to Map", () => {
  expect(Map.prototype.constructor).toBe(Map);
  expect(new Map().constructor).toBe(Map);
});

test("has the standard property descriptor", () => {
  const descriptor = Object.getOwnPropertyDescriptor(Map.prototype, "constructor");
  expect(descriptor.writable).toBe(true);
  expect(descriptor.enumerable).toBe(false);
  expect(descriptor.configurable).toBe(true);
});
