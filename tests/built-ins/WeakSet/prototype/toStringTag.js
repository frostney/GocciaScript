/*---
description: WeakSet.prototype has the correct Symbol.toStringTag
features: [WeakSet, Symbol.toStringTag]
---*/

test("WeakSet toStringTag is WeakSet", () => {
  expect(Object.prototype.toString.call(new WeakSet())).toBe("[object WeakSet]");
});

test("WeakSet prototype Symbol.toStringTag descriptor", () => {
  const desc = Object.getOwnPropertyDescriptor(WeakSet.prototype, Symbol.toStringTag);
  expect(desc.value).toBe("WeakSet");
  expect(desc.writable).toBe(false);
  expect(desc.enumerable).toBe(false);
  expect(desc.configurable).toBe(true);
});
