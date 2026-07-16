/*---
description: WeakRef constructor validates targets and exposes the expected shape
features: [WeakRef, Symbol]
---*/

test("WeakRef constructor requires new", () => {
  expect(() => WeakRef({})).toThrow(TypeError);
});

test("WeakRef constructor accepts objects and non-registered symbols", () => {
  const target = {};
  const symbol = Symbol("weak");

  expect(new WeakRef(target).deref()).toBe(target);
  expect(new WeakRef(symbol).deref()).toBe(symbol);
});

test("WeakRef constructor rejects primitives and registered symbols", () => {
  expect(() => new WeakRef()).toThrow(TypeError);
  expect(() => new WeakRef(null)).toThrow(TypeError);
  expect(() => new WeakRef(undefined)).toThrow(TypeError);
  expect(() => new WeakRef(1)).toThrow(TypeError);
  expect(() => new WeakRef("value")).toThrow(TypeError);
  expect(() => new WeakRef(true)).toThrow(TypeError);
  expect(() => new WeakRef(Symbol.for("registered"))).toThrow(TypeError);
});

test("WeakRef.length is 1 with spec descriptor", () => {
  expect(WeakRef.length).toBe(1);
  const descriptor = Object.getOwnPropertyDescriptor(WeakRef, "length");
  expect(descriptor.value).toBe(1);
  expect(descriptor.writable).toBe(false);
  expect(descriptor.enumerable).toBe(false);
  expect(descriptor.configurable).toBe(true);
});
