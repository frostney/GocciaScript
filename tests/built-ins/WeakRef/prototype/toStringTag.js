/*---
description: WeakRef.prototype has the correct Symbol.toStringTag
features: [WeakRef, Symbol.toStringTag]
---*/

test("WeakRef toStringTag is WeakRef", () => {
  expect(Object.prototype.toString.call(new WeakRef({}))).toBe("[object WeakRef]");
});

test("WeakRef prototype Symbol.toStringTag descriptor", () => {
  const desc = Object.getOwnPropertyDescriptor(WeakRef.prototype, Symbol.toStringTag);
  expect(desc.value).toBe("WeakRef");
  expect(desc.writable).toBe(false);
  expect(desc.enumerable).toBe(false);
  expect(desc.configurable).toBe(true);
});
