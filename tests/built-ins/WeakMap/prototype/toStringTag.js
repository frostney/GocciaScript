/*---
description: WeakMap.prototype has the correct Symbol.toStringTag
features: [WeakMap, Symbol.toStringTag]
---*/

test("WeakMap toStringTag is WeakMap", () => {
  expect(Object.prototype.toString.call(new WeakMap())).toBe("[object WeakMap]");
});

test("WeakMap prototype Symbol.toStringTag descriptor", () => {
  const desc = Object.getOwnPropertyDescriptor(WeakMap.prototype, Symbol.toStringTag);
  expect(desc.value).toBe("WeakMap");
  expect(desc.writable).toBe(false);
  expect(desc.enumerable).toBe(false);
  expect(desc.configurable).toBe(true);
});
