/*---
description: FinalizationRegistry constructor validates cleanup callbacks and exposes the expected shape
features: [FinalizationRegistry]
---*/

test("FinalizationRegistry constructor requires new", () => {
  expect(() => FinalizationRegistry(() => {})).toThrow(TypeError);
});

test("FinalizationRegistry constructor accepts callable cleanup callbacks", () => {
  const registry = new FinalizationRegistry(() => {});
  expect(registry instanceof FinalizationRegistry).toBe(true);
});

test("FinalizationRegistry constructor rejects non-callable cleanup callbacks", () => {
  expect(() => new FinalizationRegistry()).toThrow(TypeError);
  expect(() => new FinalizationRegistry(undefined)).toThrow(TypeError);
  expect(() => new FinalizationRegistry(null)).toThrow(TypeError);
  expect(() => new FinalizationRegistry({})).toThrow(TypeError);
  expect(() => new FinalizationRegistry(1)).toThrow(TypeError);
  expect(() => new FinalizationRegistry("callback")).toThrow(TypeError);
});

test("FinalizationRegistry.length is 1 with spec descriptor", () => {
  expect(FinalizationRegistry.length).toBe(1);
  const descriptor = Object.getOwnPropertyDescriptor(FinalizationRegistry, "length");
  expect(descriptor.value).toBe(1);
  expect(descriptor.writable).toBe(false);
  expect(descriptor.enumerable).toBe(false);
  expect(descriptor.configurable).toBe(true);
});
