/*---
description: Error.prototype.message
features: [Error]
---*/

test("has the standard descriptor", () => {
  const descriptor = Object.getOwnPropertyDescriptor(Error.prototype, "message");
  expect(descriptor.enumerable).toBe(false);
  expect(descriptor.writable).toBe(true);
  expect(descriptor.configurable).toBe(true);
});
