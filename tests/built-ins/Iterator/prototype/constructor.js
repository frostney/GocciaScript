/*---
description: Iterator.prototype.constructor
features: [Iterator, iterator-helpers]
---*/

test("is an accessor returning Iterator", () => {
  const descriptor = Object.getOwnPropertyDescriptor(
    Iterator.prototype,
    "constructor",
  );
  expect(typeof descriptor.get).toBe("function");
  expect(typeof descriptor.set).toBe("function");
  expect(descriptor.enumerable).toBe(false);
  expect(descriptor.configurable).toBe(true);
  expect(descriptor.value).toBe(undefined);
  expect(descriptor.writable).toBe(undefined);
  expect(Iterator.prototype.constructor).toBe(Iterator);
  expect(descriptor.get.call()).toBe(Iterator);
});
