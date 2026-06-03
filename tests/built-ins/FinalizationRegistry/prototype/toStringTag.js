/*---
description: FinalizationRegistry.prototype has the correct Symbol.toStringTag
features: [FinalizationRegistry, Symbol.toStringTag]
---*/

test("FinalizationRegistry toStringTag is FinalizationRegistry", () => {
  const registry = new FinalizationRegistry(() => {});
  expect(Object.prototype.toString.call(registry)).toBe("[object FinalizationRegistry]");
});

test("FinalizationRegistry prototype Symbol.toStringTag descriptor", () => {
  const desc = Object.getOwnPropertyDescriptor(
    FinalizationRegistry.prototype,
    Symbol.toStringTag
  );
  expect(desc.value).toBe("FinalizationRegistry");
  expect(desc.writable).toBe(false);
  expect(desc.enumerable).toBe(false);
  expect(desc.configurable).toBe(true);
});
