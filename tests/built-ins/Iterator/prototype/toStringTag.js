/*---
description: Iterator.prototype Symbol.toStringTag
features: [Iterator, iterator-helpers, Symbol.toStringTag]
---*/

test("is an accessor returning Iterator", () => {
  const descriptor = Object.getOwnPropertyDescriptor(
    Iterator.prototype,
    Symbol.toStringTag,
  );
  expect(typeof descriptor.get).toBe("function");
  expect(typeof descriptor.set).toBe("function");
  expect(descriptor.enumerable).toBe(false);
  expect(descriptor.configurable).toBe(true);
  expect(descriptor.value).toBe(undefined);
  expect(descriptor.writable).toBe(undefined);
  expect(Iterator.prototype[Symbol.toStringTag]).toBe("Iterator");
  expect(Object.prototype.toString.call(Iterator.prototype)).toBe(
    "[object Iterator]",
  );
});
